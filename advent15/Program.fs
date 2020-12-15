open System
open Xunit

let test_1 = [ 0; 3; 6]
type Spoken =  SpokenOnce of int | SpokenTwice of (int*int)

let rec yieldNext (m: Map<int,Spoken>) (last: int) (turn: int)  : (int*int) seq =
    seq {
        let lastSeenOpt = Map.tryFind last m
        let next = match lastSeenOpt with
                            | None -> 0 
                            | Some (SpokenOnce lastseen) -> 0 
                            | Some (SpokenTwice (lastseen, firstseen)) -> lastseen - firstseen 

        yield (turn, next)
        let findNewNr = Map.tryFind next m
        let newState = match findNewNr with
                            | None -> SpokenOnce turn 
                            | Some (SpokenOnce lastseen) -> SpokenTwice (turn, lastseen) 
                            | Some (SpokenTwice (lastseen, firstseen)) -> SpokenTwice (turn, lastseen) 
        let m' = Map.add next newState m
        yield! yieldNext m' next (turn + 1)
    }

let game (input: int list) : (int*int) seq =
    let foo = input |> List.mapi (fun i  -> fun e -> (e, SpokenOnce (i+1)))
    let m = Map.ofSeq foo
    seq {
        for (key, value) in foo do
            match value with | SpokenOnce foo -> yield (foo, key) 
        yield! yieldNext m (input |> Seq.last) (foo.Length + 1)
    } 

let getNthElement (elemntNr: int) (sekvens: (int*int) seq) : int*int = 
    sekvens |> Seq.nth (elemntNr - 1) 

[<Fact>]
let ``firstSequence nr 2020`` () =
    let (turn, nr)= game test_1 |> getNthElement 2020
    Assert.Equal(2020, turn)
    Assert.Equal(436, nr)

[<Fact>]
let ``secondSequence nr 2020`` () =
    let (_, nr)= game [1;3;2] |> getNthElement 2020
    Assert.Equal(1, nr)

    let (_, nr)= game [2;1;3] |> getNthElement 2020
    Assert.Equal(10, nr)
    let (_, nr)= game [1;2;3] |> getNthElement 2020
    Assert.Equal(27, nr)
    let (_, nr)= game [2;3;1] |> getNthElement 2020
    Assert.Equal(78, nr)
    let (_, nr)= game [3;2;1] |> getNthElement 2020
    Assert.Equal(438, nr)
    let (_, nr)= game [3;1;2] |> getNthElement 2020
    Assert.Equal(1836, nr)

    let (_, nr)= game [1;2;16;19;18;0] |> getNthElement 2020
    Assert.Equal(536, nr)

//    let largeNr = 30000000 
//    let (_, nr)= game [0;3;6] |> getNthElement largeNr 
//      Assert.Equal(175594, nr)

[<EntryPoint>]
let main argv =
    let largeNr = 30000000 
    let (_, nr)= game [1;2;16;19;18;0] |> getNthElement largeNr

    printfn "The answer is %A" nr 
    0 
