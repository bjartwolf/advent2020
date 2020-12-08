open Domain
open Parser 

[<EntryPoint>]
let main argv =
    let file = readInputfile "data/input.txt"
    let parsed = parseLines file
    printf "%A" parsed
    0
