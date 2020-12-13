open System
open Xunit

type Input = int [] 

let parseInput (file: string) : Input = 
    let i_raw = IO.File.ReadAllLines file
    let line1 = (i_raw.[1]).Split(",")
    line1 |> Array.where(fun l -> not (l = "x")) |> Array.map (int)

[<Fact>]
let ``parse input``() =
    let routes = parseInput "data/test_1.txt"
    Assert.Contains(13, routes)
    let busesMatchInput  = ([|7;13;59;31;19|] = routes)
    Assert.True(busesMatchInput)
    
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
