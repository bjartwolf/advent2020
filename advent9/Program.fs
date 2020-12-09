open System
open Xunit


let validValues (inputSeq: seq<int>) = seq {
    for value1 in inputSeq do
        for value2 in inputSeq do
            if not (value1 = value2) then
                yield value1 + value2
    }

let oneTo25Seq = seq { 1.. 25 }

[<Fact>]
let ``does contains 26`` () =
    Assert.Contains(26, validValues oneTo25Seq)

[<Fact>]
let ``not contains 50`` () =
    Assert.DoesNotContain(50, validValues oneTo25Seq)

[<Fact>]
let ``not contains 100`` () =
    Assert.DoesNotContain(100, validValues oneTo25Seq)

[<Fact>]
let ``contains 49`` () =
    Assert.Contains(49, validValues oneTo25Seq)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#! %A" oneTo25Seq 
    0
