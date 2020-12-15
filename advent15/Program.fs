﻿open System
open Xunit

let test_1 = seq { 0; 3; 6}

let rec yieldNext (m: Map<int,int>) (input: int) : int seq =
    seq {
        yield input
        yield! yieldNext m input
    }

let game (input: int seq) : int seq =
    let m = Map.empty 
    seq {
        yield! input
        yield! yieldNext m 1
    } 

[<Fact>]
let ``firstSequence`` () =
    let answer = [0; 3 ; 6] 
    let foo = game test_1 |> Seq.take 5
    Assert.Equal(answer, foo)


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 
