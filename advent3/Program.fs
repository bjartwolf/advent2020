open System
open System.IO

type Point = { X: int;
               Y: int } 

type Vector = Point

type Tree = Tree
type Rows = Tree option array 
type Map = Rows array 

let move (from: Point) (direction: Vector) =
    { X = from.X + direction.X;
      Y = from.Y + direction.Y }

let parseLines (lines: string array): Map =
    let charToVeg c = 
        c |> Seq.map (fun c -> match c with
                                   | '#' -> Some Tree 
                                   | _ -> None) 
          |> Seq.toArray
    lines |> Array.map ((charToVeg))

let getTerrain (map: Map) (pos: Point) = map.[pos.X].[pos.Y]

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input_trees.txt"
    let map = parseLines input

    let pos = { X= 0; Y = 0}
    let dir = { X = 1; Y = 3 } // it seems transposed

    let countTrees pos direction map nrOfTrees = 
        let sizeX = Array.length map
        let sizeY = Array.length map.[0]
        let rec moveInMap pos direction map nrOfTrees = 
            let newPosition = move pos direction 
            if (newPosition.X >= sizeX) then
                 nrOfTrees 
            else 
                let wrappedPosition = { newPosition with Y = newPosition.Y % sizeY }
                let terrain = getTerrain map wrappedPosition 
                match terrain with
                    | None -> moveInMap wrappedPosition direction map nrOfTrees
                    | Some Tree -> moveInMap wrappedPosition direction map (nrOfTrees + 1)
        moveInMap pos direction map nrOfTrees

    printf "Challenge 1: Number of trees %i \n" (countTrees pos dir map 0)

    let slopes = [ { X=1 ; Y=1 };
                   { X=1 ; Y=3 };
                   { X=1 ; Y=5 };
                   { X=1 ; Y=7 };
                   { X=2 ; Y=1 }]
    let nrOfTrees = slopes |> List.map(fun s-> countTrees pos s map 0)
    let product = nrOfTrees |> List.reduce (( * )) 

    printf "Challenge 2: Product of trees %i" product
    0