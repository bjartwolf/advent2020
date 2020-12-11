// Learn more about F# at http://fsharp.org

open System
open Xunit 

type seatState = Empty | Occ | Floor
type Seats =  Map<int*int, seatState>
type Plane = { width : int;
               height: int;
               seats: Seats }

let readSeats file : Map<int*int, seatState> = 
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
    m

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

let adjacentSeats (seats: Seats) (pos: int*int) : Seats =
    seats |> Map.filter (fun seatPos -> fun _ -> isAdjacent seatPos pos) 

let countEmpty = countStates Empty
let countOcc = countStates Occ 
let countFloor = countStates Floor 
let countAll seats = countEmpty seats + countOcc seats + countFloor seats 

let calcNewState (pos: int*int) (seats: Seats) : seatState =
    let adjacent =  adjacentSeats seats 
    let seat = Map.find pos seats
    let nrOfOccupied = countOcc (adjacent pos)
    match seat, nrOfOccupied with 
        | Occ, nrOfOccupied when nrOfOccupied >=4  -> Empty 
        | Empty, 0 -> Occ
        | s,_ -> s 

let calcNewboardState (seats: Seats) : Seats =
    seats |> Map.map (fun seatPos -> fun _ -> calcNewState seatPos seats) 

let rec runSimulationUntilCompletion (seats: Seats) : Seats =
    let newState = calcNewboardState seats
    if (newState = seats) then
        seats
    else
        runSimulationUntilCompletion newState

[<Fact>]
let ``Calculating occupied seats works for testdata``() =
    Assert.Equal(37, runSimulationUntilCompletion testState |> countOcc)
 
[<Fact>]
let ``Calculating entire simulation works for testdata``() =
    let endState = runSimulationUntilCompletion testState
    let endStateReached = endState = testState5
    Assert.True(endStateReached)

    (*
[<Fact>]
let ``Calculating entire simulation works for input``() =
    Assert.Equal(2, runSimulationUntilCompletion (readSeats "data/input.txt") |> countOcc)
 *) 
[<Fact>]
let ``Calculating new state works for testdata``() =
    Assert.True(calcNewboardState testState = testState1)
    Assert.True(calcNewboardState testState1 = testState2)
    Assert.True(calcNewboardState testState2 = testState3)
    Assert.True(calcNewboardState testState3 = testState4)
    Assert.True(calcNewboardState testState4 = testState5)
    
[<Fact>]
let ``Calculating new state works``() =
    Assert.Equal(Occ, calcNewState (0,0) testState)
    Assert.Equal(Occ, calcNewState (1,1) testState)
    Assert.Equal(Occ, calcNewState (1,0) testState)
    Assert.Equal(Floor, calcNewState (0,1) testState)
    Assert.Equal(Empty, calcNewState (0,3) testState1)
 
[<Fact>]
let ``Adjant seat counts fairly accurate with states``() =
    let adjacent =  adjacentSeats testState  
    Assert.Equal(2, countEmpty (adjacent (0,0)))
    Assert.Equal(1, countFloor (adjacent (0,0)))
    Assert.Equal(0, countOcc (adjacent (0,0)))
    Assert.Equal(6, countEmpty (adjacent (1,1)))
    Assert.Equal(2, countFloor (adjacent (1,1)))
    Assert.Equal(5, countEmpty (adjacent (9,5)))
  
[<Fact>]
let ``Adjant seat counts fairly accurate``() =
    let adjacent =  adjacentSeats testState  
    Assert.Equal(3, countAll (adjacent (0,0)))
    Assert.Equal(8, countAll (adjacent (1,1)))
    Assert.Equal(3, countAll (adjacent (9,9)))
    Assert.Equal(5, countAll (adjacent (8,9)))
    Assert.Equal(3, countAll (adjacent (0,9)))
    Assert.Equal(8, countAll (adjacent (8,8)))

[<Fact>]
let ``Total 100``() =
    Assert.Equal(100, countEmpty testState + countFloor testState + countOcc testState)
    Assert.Equal(100, countAll testState) 

[<EntryPoint>]
let main argv =
    let sum = runSimulationUntilCompletion (readSeats "data/input.txt") |> countOcc
    printfn "occupied %A" sum 
    0 // return an integer exit codnse
