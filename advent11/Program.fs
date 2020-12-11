// Learn more about F# at http://fsharp.org

open System
open Xunit 

type seatState = Empty | Occ | Floor
type boardSize = int * int 
type Seats =  Map<int*int, seatState>
type Plane = { width : int;
               height: int;
               seats: Seats }

let readSeats file : Map<int*int, seatState> * boardSize = 
    let rows = System.IO.File.ReadAllLines file
    let length = rows.Length
    let width = rows.[0].Length
    let mutable m = Map.empty
    for row in 0 .. length - 1 do
            for column in 0 .. width - 1 do
                let seat = rows.[row].[column] 
                let seat' = match seat with 
                                | '.' -> Floor
                                | 'L' -> Empty
                                | '#' -> Occ
                m <- Map.add (row, column) seat' m 
    (m,(length,width))

let countStates (state: seatState) (seats: Seats) : int = 
    seats |> Seq.where (fun s -> s.Value = state) |> Seq.length 

let isAdjacent (x1:int, y1: int) (x2:int, y2: int) =
    let xdist = Math.Abs (x1-x2)
    let ydist = Math.Abs (y1-y2)
    match xdist, ydist with
        | 1,0 -> true
        | 0,1 -> true
        | 1,1 -> true
        | 0,0 -> false
        | _ -> false

[<Fact>]
let ``Are adjacent``() =
    Assert.True(isAdjacent (1,1) (2,1))
    Assert.True(isAdjacent (2,2) (2,1))
    Assert.False(isAdjacent (3,1) (1,1))
    Assert.False(isAdjacent (2,1) (2,1))
    Assert.True(isAdjacent (-1,1) (0,1))

let testState = readSeats "data/testdata_round_0.txt"
let testState1 = readSeats "data/testdata_round_1.txt"
let testState2 = readSeats "data/testdata_round_2.txt"
let testState3 = readSeats "data/testdata_round_3.txt"
let testState4 = readSeats "data/testdata_round_4.txt"
let testState5 = readSeats "data/testdata_round_5.txt"

let adjacentSeats (seats: Seats) (boardSize: boardSize) ((x,y): int*int) : Seats =
    // speed it up by looking at only the possible seats
    let (sizeX,sizeY) = boardSize

    let posssiblePositions  = [x+1,y;
                               x-1,y;
                               x,y+1;
                               x,y-1;
                               x-1,y-1;
                               x+1,y+1;
                               x-1,y+1;
                               x+1,y-1]
    let positions = posssiblePositions |> List.filter (fun (x,y) -> x >= 0 && x < sizeX && y >= 0 && y < sizeY)
//    let positions = posssiblePositions |> List.filter (fun seatPos -> isAdjacent seatPos (x,y) )
    positions |> List.map (fun e -> (e, Map.find e seats)) |> Map.ofList

let adjacentSeatsV2 (seats: Seats) (boardSize: boardSize) ((x,y): int*int) : Seats =
    // speed it up by looking at only the possible seats
    let (sizeX,sizeY) = boardSize

    let mult (x,y) scale =
        (x*scale, y*scale)
    let addVec (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    let posssibleDirections = [0+1,0;
                               0-1,0;
                               0,0+1;
                               0,0-1;
                               0-1,0-1;
                               0+1,0+1;
                               0-1,0+1;
                               0+1,0-1]
    let rec walk (dirx:int,diry:int) (scale:int) : ((int*int)* seatState) option =
        let pos = addVec (x,y) (mult (dirx, diry) scale)
        let (x1, y1) = pos
        if not (x1 >= 0 && x1 < sizeX && y1 >= 0 && y1 < sizeY) then 
             None // none? what do we get outside? Just not add it?
        else 
            let check = Map.find pos seats
            match check with 
                | Floor -> walk (dirx,diry) (scale+ 1)
                | Empty -> Some (pos, Empty)
                | Occ -> Some (pos, Occ)

    posssibleDirections |> List.map (fun dir -> walk dir 1) |> List.choose id
              |> Map.ofList 
 //   positions |> List.map (fun e -> (e, Map.find e seats)) |> Map.ofList
    
 

let countEmpty = countStates Empty
let countOcc = countStates Occ 
let countFloor = countStates Floor 
let countAll seats = countEmpty seats + countOcc seats + countFloor seats 

let calcNewState (pos: int*int) (boardSize: int*int) (seats: Seats) : seatState =
    let adjacent =  adjacentSeats seats boardSize 
    let seat = Map.find pos seats
    let nrOfOccupied = countOcc (adjacent pos)
    match seat, nrOfOccupied with 
        | Occ, nrOfOccupied when nrOfOccupied >=4  -> Empty 
        | Empty, 0 -> Occ
        | s,_ -> s 

let calcNewboardState (seats: Seats) (boardSize: int*int) : Seats =
    seats |> Map.map (fun seatPos -> fun _ -> calcNewState seatPos boardSize seats) 

    // make an inner loop
let rec runSimulationUntilCompletion (seats: Seats, size: boardSize) : Seats =
    let newState = calcNewboardState seats size
    if (newState = seats) then
        seats
    else
        runSimulationUntilCompletion (newState, size)

[<Fact>]
let ``Calculating occupied seats works for testdata``() =
    Assert.Equal(37, runSimulationUntilCompletion testState |> countOcc)
 
[<Fact>]
let ``Calculating entire simulation works for testdata``() =
    let (initSeats, boardSize) = testState
    let (endSeats, _) = testState5
    let endState = runSimulationUntilCompletion (initSeats, boardSize)
    let endStateReached = endState = endSeats 
    Assert.True(endStateReached)

    (*
[<Fact>]
let ``Calculating entire simulation works for input``() =
    Assert.Equal(2, runSimulationUntilCompletion (readSeats "data/input.txt") |> countOcc)
 *) 
[<Fact>]
let ``Calculating new state works for testdata``() =
    let (initSeats, boardSize) = testState
    let (s1,_) = testState1
    let (s2,_) = testState2
    let (s3,_) = testState3
    let (s4,_) = testState4
    let (s5,_) = testState5
    Assert.True(calcNewboardState initSeats boardSize = s1)
    Assert.True(calcNewboardState s1 boardSize = s2)
    Assert.True(calcNewboardState s2 boardSize = s3)
    Assert.True(calcNewboardState s3 boardSize = s4)
    Assert.True(calcNewboardState s4 boardSize = s5)
    
[<Fact>]
let ``Calculating new state works``() =
    let (initSeats, _) = testState
    let (s1,_) = testState1
    Assert.Equal(Occ, calcNewState (0,0) (9,9) initSeats)
    Assert.Equal(Occ, calcNewState (1,1) (9,9) initSeats)
    Assert.Equal(Occ, calcNewState (1,0) (9,9) initSeats)
    Assert.Equal(Floor, calcNewState (0,1) (9,9) initSeats)
    Assert.Equal(Empty, calcNewState (0,3) (9,9) s1)
 
[<Fact>]
let ``Adjant seat counts fairly accurate with states``() =
    let (s0, boardSize) = testState
    let adjacent =  adjacentSeats s0 boardSize
    Assert.Equal(2, countEmpty (adjacent (0,0)))
    Assert.Equal(1, countFloor (adjacent (0,0)))
    Assert.Equal(0, countOcc (adjacent (0,0)))
    Assert.Equal(6, countEmpty (adjacent (1,1)))
    Assert.Equal(2, countFloor (adjacent (1,1)))
    Assert.Equal(5, countEmpty (adjacent (9,5)))

[<Fact>]
let ``See seats``() =
   let (b,size) = readSeats "data/part2_testdata_0.txt"
   let canSeeSeats = adjacentSeatsV2 b size (4,3) 
   Assert.Equal(8, canSeeSeats |> countOcc) 

[<Fact>]
let ``See seats2``() =
   let (b,size) = readSeats "data/part2_testdata_1.txt"
   let canSeeSeats = adjacentSeatsV2 b size (1,1) 
   Assert.Equal(1, canSeeSeats |> countAll) 
   Assert.Equal(1, canSeeSeats |> countEmpty) 
   Assert.Equal(0, canSeeSeats |> countOcc) 

[<Fact>]
let ``See seats3``() =
   let (b,size) = readSeats "data/part2_testdata_2.txt"
   let canSeeSeats = adjacentSeatsV2 b size (3,3) 
   Assert.Equal(0, canSeeSeats |> countAll) 
    
[<Fact>]
let ``Adjant seat counts fairly accurate``() =
    let (s0, boardSize) = testState
    let adjacent =  adjacentSeats s0 boardSize 
    Assert.Equal(3, countAll (adjacent (0,0)))
    Assert.Equal(8, countAll (adjacent (1,1)))
    Assert.Equal(3, countAll (adjacent (9,9)))
    Assert.Equal(5, countAll (adjacent (8,9)))
    Assert.Equal(3, countAll (adjacent (0,9)))
    Assert.Equal(8, countAll (adjacent (8,8)))

[<Fact>]
let ``Total 100``() =
    let (s0, boardSize) = testState
    Assert.Equal((10,10), boardSize)
    Assert.Equal(100, countEmpty s0 + countFloor s0 + countOcc s0)
    Assert.Equal(100, countAll s0) 

[<EntryPoint>]
let main argv =
    let sum = runSimulationUntilCompletion (readSeats "data/input.txt") |> countOcc
    printfn "occupied %A" sum 
    0 // return an integer exit codnse
