open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Point = { X: int;
               Y: int } 

type Vector = Point

type Vegetation = Tree | Snow
type Rows = Vegetation array 
type Map = Rows array 

let move (from: Point) (direction: Vector) =
    { X = from.X + direction.X;
      Y = from.Y + direction.Y }

let parseLines (lines: string seq): Map =

    let charToVeg c = 
        c |> Seq.map (fun c -> match c with
                               | '#' -> Tree 
                               | _ -> Snow)
    lines 
          |> Seq.map (fun s -> s |> charToVeg) 
          |> Seq.map Seq.toArray 
          |> Seq.toArray

let getTerrain (map: Map) (pos: Point) =
    let sizeX = map.Length
    let sizeY = map.[0].Length
    // wrap Y
    let y = pos.Y % sizeY
    if (pos.X >= sizeX) then
        None
    else 
        Some map.[pos.X].[y]

[<EntryPoint>]
let main argv =
    let input = readLines "input_trees.txt"
    let map = parseLines input

    let pos = { X= 0; Y = 0}
    let dir = { X = 1; Y = 3 } // it seems transposed

    let rec moveInMap startPos direction map nrOfTrees = 
        let newPosition = move startPos direction 
        let terrain = getTerrain map newPosition
        match terrain with
            | Some Snow -> moveInMap newPosition direction map nrOfTrees
            | Some Tree -> moveInMap newPosition direction map (nrOfTrees + 1)
            | None -> nrOfTrees 
    printf "Challenge 1: Number of trees %i \n" (moveInMap pos dir map 0)

    let slopes = [ { X=1 ; Y=1 };
                   { X=1 ; Y=3 };
                   { X=1 ; Y=5 };
                   { X=1 ; Y=7 };
                   { X=2 ; Y=1 }]
    let nrOfTrees = slopes |> List.map(fun s-> moveInMap pos s map 0)
    let product = nrOfTrees |> List.reduce (( * )) 

    printf "Challenge 2: Product of trees %i" product
    0