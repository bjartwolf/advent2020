open Xunit
open FSharp.Collections.ParallelSeq

let validValues (inputSeq: int64 list) = 
    let valid = seq {
            for i in 0 .. inputSeq.Length - 1 do
                for j in 00 .. inputSeq.Length - 1 do
                    if i > j then 
                        yield inputSeq.[i] + inputSeq.[j] 
            }
    valid |> Seq.toList

type AddResult = Success of int64 list | Failure 

let addIfValid (newValue: int64) (sequence: int64 list ) : AddResult =
    if List.contains newValue (validValues sequence) then
        Success ((List.tail sequence) @ [newValue])
    else 
        Failure

let rec takeUntilFailure (preamble: int64 list) (sequence: int64 list) : int64 =
    let res = addIfValid (Seq.head sequence) preamble 
    match res with 
        | Success lst -> takeUntilFailure lst (List.skip 1 sequence)
        | Failure -> Seq.head sequence

let findEncryptionWeakness (answer:int64) (numbers: int64 list) = 
    seq {
        for i in seq { 2 .. numbers.Length } do 
            let lists = List.windowed i numbers
            let answers = lists |> List.where (fun l -> (List.sum l) = answer) 
            if not (List.isEmpty answers) then
               let found = List.head answers
               yield List.max found + List.min found
    } |> Seq.head

[<Fact>]
let ``is valid until 127`` () =
    let smallerNumberRange = System.IO.File.ReadAllLines("data/20_input.txt") 
                                |> Array.map (int64) 
                                |> Array.toList
    let preamble = smallerNumberRange |> List.take 5
    let sequence = smallerNumberRange |> List.skip 5 
    let firstFailure = takeUntilFailure preamble sequence

    Assert.Equal(127L, firstFailure)

let oneTo25Seq = seq { 1L.. 25L } |> Seq.toList

[<Fact>]
let ``find encryption weakness for 1504371145L`` () =
    let nums = System.IO.File.ReadAllLines("data/large_input.txt") 
                                |> Array.map (int64) 
                                |> Array.toList
    Assert.Equal(183278487L, findEncryptionWeakness 1504371145L nums) 

[<Fact>]
let ``find encryption weakness for 127`` () =
    let smallerNumberRange = System.IO.File.ReadAllLines("data/20_input.txt") 
                                |> Array.map (int64) 
                                |> Array.toList
    Assert.Equal(62L, findEncryptionWeakness 127L smallerNumberRange) 

[<Fact>]
let ``is valid until 123337`` () =
    let smallerNumberRange = System.IO.File.ReadAllLines("data/large_input.txt") 
                                |> Array.map (int64) 
                                |> Array.toList
    let preambleLength = 25
    let preamble = smallerNumberRange |> List.take preambleLength 
    let sequence = smallerNumberRange |> List.skip preambleLength 
    let firstFailure = takeUntilFailure preamble sequence

    Assert.Equal(1504371145L, firstFailure)

[<Fact>]
let ``can add 26`` () =
    let res = addIfValid 26L oneTo25Seq
    Assert.Equal(Success [2L .. 26L], res)

[<Fact>]
let ``can add 45, 64, 66, but not 65`` () =
    let startSequence = [1L..19L]@[21L..25L]
    
    Assert.Equal(Success (List.tail startSequence @ [26L]), addIfValid 26L startSequence)

    let res = addIfValid 45L startSequence 
    Assert.Equal(Success (List.tail startSequence @ [45L]), res)

    let lst = match res with
                            | Success a -> a 
                            | _ -> failwith "no list" 
                        
    Assert.Equal(Success (List.tail (List.tail startSequence) @[45L;64L]), addIfValid 64L lst)
    Assert.Equal(Success (List.tail (List.tail startSequence) @[45L;66L]), addIfValid 66L lst)
    Assert.Equal(Failure, addIfValid 65L lst)

[<Fact>]
let ``does contains 26`` () =
    Assert.Contains(26L, validValues oneTo25Seq)

[<Fact>]
let ``not contains 50`` () =
    Assert.DoesNotContain(50L, validValues oneTo25Seq)

[<Fact>]
let ``not contains 100`` () =
    Assert.DoesNotContain(100L, validValues oneTo25Seq)

[<Fact>]
let ``contains 49`` () =
    Assert.Contains(49L, validValues oneTo25Seq)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#! %A" oneTo25Seq 
    0
