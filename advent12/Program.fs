// Learn more about F# at http://fsharp.org

open System
open Xunit

[<Fact>]
let ``foo`` () =
    let foo = IO.File.ReadAllLines "data/test2.txt"
    Assert.Equal(1, foo |> Array.length)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
