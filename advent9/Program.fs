open System
open Xunit

let validValues (inputSeq: int list) = 
    let valid = seq {
            for value1 in inputSeq do
                for value2 in inputSeq do
                    if not (value1 = value2) then
                        yield value1 + value2
            }
    valid |> Seq.toList

let oneTo25Seq = seq { 1.. 25 } |> Seq.toList

type AddResult = Success of int list | Failure 

let addIfValid (newValue: int) (sequence: int list ) : AddResult =
    if List.contains newValue (validValues sequence) then
        Success ((List.tail sequence) @ [newValue])
    else 
        Failure

[<Fact>]
let ``can add 26`` () =
    let res = addIfValid 26 oneTo25Seq
    Assert.Equal(Success [2 .. 26], res)

[<Fact>]
let ``can add 45, 64, 66, but not 65`` () =
    let startSequence = [1..19]@[21..25]
    
    Assert.Equal(Success (List.tail startSequence @ [26]), addIfValid 26 startSequence)

    let res = addIfValid 45 startSequence 
    Assert.Equal(Success (List.tail startSequence @ [45]), res)

    let lst = match res with
                            | Success a -> a 
                            | _ -> failwith "no list" 
                        
    Assert.Equal(Success (List.tail (List.tail startSequence) @[45;64]), addIfValid 64 lst)
    Assert.Equal(Success (List.tail (List.tail startSequence) @[45;66]), addIfValid 66 lst)
    Assert.Equal(Failure, addIfValid 65 lst)

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
