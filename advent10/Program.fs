open System
open Xunit 

// adapter can 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
// your device has a built-in joltage adapter rated for 3 jolts higher than the 
//highest-rated adapter in your bag
type Adapters = int list

let rating (adapters: Adapters) =
   List.max adapters + 3 

[<Fact>]
let ``3 6 and 9 rates adapter to 12`` () = 
    let adapters = [3;6;9] 
    Assert.Equal(12, rating adapters)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
