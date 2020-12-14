open System
open Xunit

open Parser

// kanskje minnet kan være en map
type Memory = Map<int, int>
// instruksjonene 
//The current bitmask is applied to values immediately before they are written to memory
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
