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

[<Fact>]
let ``firstSequence`` () =
    let answer = [(1,0); (2,3) ; (3,6); (4,0); (5,3); (6,3); (7,1); (8,0) ; (9,4); (10,0) ] 
    let foo = game test_1 |> Seq.take 10 |> Seq.toList
    printfn "answer %A" answer
    printfn "foo %A" foo
    let isMatch = answer = foo
    Assert.True(isMatch)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 
