open System

let mapCharToValue i character = match character with
                                        | 'F' | 'L' -> 0 
                                        | 'R' | 'B' -> pown 2 i 
                                        | _ -> failwith "input not handled" 

let parseSeat seatCode = seatCode 
                            |> Seq.rev
                            |> Seq.mapi mapCharToValue
                            |> Seq.sum 

[<EntryPoint>]
let main argv =
    // Unittests as inspired by Bårds comment yesterday
    assert ((parseSeat "BBB") = 7)
    assert ((parseSeat "FLF") = 0)
    assert ((parseSeat "RBR") = 7)
    assert ((parseSeat "L") = 0)
    assert ((parseSeat "RL") = 2)
    assert ((parseSeat "RB") = 3)
    assert ((parseSeat "FBFBBFFRLR") = 357)
    //

    System.IO.File.ReadAllLines("input5.txt") 
        |> Seq.map (parseSeat)
        |> Seq.max 
        |> printf "The max value is %i" 
    0