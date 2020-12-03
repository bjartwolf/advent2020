open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Point = { X: int;
               Y: int } 

type Vegetation = Tree | Snow
type Columns = Vegetation array 
type Map = Columns array 

let parseLines (lines: string seq): Map =
    let charSnow = Convert.ToChar(".") // implicit let charTree = Convert.ToChar("#")
    let stringToChar (s: string seq) = s |> Seq.map (fun s -> s.ToCharArray())
    let (|Tree|Snow|) character = if character = charSnow then Snow else Tree 

    let charToVeg c = 
        c |> Seq.map (fun c -> match c with
                               | Snow -> Snow
                               | Tree -> Tree)
    lines |> stringToChar 
          |> Seq.map (fun s -> s |> charToVeg) 
          |> Seq.map Seq.toArray 
          |> Seq.toArray

let getTerrain (map: Map) (pos: Point) =
    map.[pos.X].[pos.Y]

[<EntryPoint>]
let main argv =
    let message = readLines "input_trees.txt"
    printfn "Hello world %A" (parseLines message)
    let pos = { X= 0; Y = 0}
    0