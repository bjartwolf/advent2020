open System
open Xunit

[<AutoOpen>]
module Common =
    let curry f = (fun a b -> f(a,b))
    let uncurry f = (fun (a,b) -> f a b)

    let zipWith f ls l's =
        let rec inner ls l's cont =
            match ls, l's with
            | [], []         -> cont []
            | l::ls, l'::l's -> inner ls l's (fun r -> (f l l')::r |> cont)
            | _              -> failwith "Lists don't have same length"
        inner ls l's (fun r -> r)

    let zipWith3 f ls l's l''s =
        let rec inner ls l's l''s cont =
            match ls, l's, l''s with
            | [], [], []                -> cont []
            | l::ls, l'::l's, l''::l''s -> inner ls l's l''s (fun r -> (f l (f l' l''))::r |> cont)
            | _                         -> failwith "Lists don't have same length"
        inner ls l's l''s (fun r -> r)

// Route and offsetmodr, makes int64 to make it easier to compare 
type Bus = int64 * int64 * int64
type Input = Bus [] 

let parseInput (file: string) : Input = 
    let i_raw = IO.File.ReadAllLines file
    let line1 = (i_raw.[1]).Split(",")
    line1 |> Array.mapi (fun o -> fun s -> (o,s))
          |> Array.where(fun (_,s) -> not (s = "x"))
          |> Array.map (fun (o,r) -> int64 r, int64 o)
          |> Array.map (fun (r,o) -> if o = 0L then (o,r, 0L) else (o,r, o % r))

let verifySolution (i: (int64*int64*int64) []) (ts: int64) : bool =
    // teste med at r er produktet av alle tallene
    // det er noe med å bare sjekke for de tallene, ikke lage sekvenser for alt annet
    i |> Array.forall (fun (_,r, omodr) -> let tsr = (ts % r)
                                           (tsr + omodr = r || tsr + omodr = 0L))

let solutions (i: Input) = 
    let (_,r,_) = i.[0]
    let verifyer = verifySolution i 
    seq {
       for ts in 0L .. r .. Int64.MaxValue do if (verifyer ts) then ts  
    }

[<Fact>]
let ``examples`` () =
    Assert.True(verifySolution (parseInput "data/test_1.txt") 1068781L)
    Assert.Equal(solutions (parseInput "data/test_1.txt") |> Seq.head, 1068781L)
    Assert.True(verifySolution (parseInput "data/test_2.txt") 3417L)
    Assert.True(verifySolution (parseInput "data/test_3.txt") 754018L)
    Assert.True(verifySolution (parseInput "data/test_4.txt") 779210L)
    Assert.Equal(solutions (parseInput "data/test_4.txt") |> Seq.head, 779210L)
    Assert.True(verifySolution (parseInput "data/test_5.txt") 1261476L)
    Assert.Equal(solutions (parseInput "data/test_5.txt") |> Seq.head, 1261476L)
    Assert.True(verifySolution (parseInput "data/test_6.txt") 1202161486L)
    Assert.True(verifySolution (parseInput "data/input.txt") 210612924881803L)

[<Fact>]
let ``counter examples`` () =
    Assert.False(verifySolution (parseInput "data/test_1.txt") 1068780L)
    Assert.False(verifySolution (parseInput "data/test_2.txt") 3416L)
    Assert.False(verifySolution (parseInput "data/test_3.txt") 754017L)
    Assert.False(verifySolution (parseInput "data/test_4.txt") 779200L)
    Assert.False(verifySolution (parseInput "data/test_5.txt") 1261376L)
    Assert.False(verifySolution (parseInput "data/test_6.txt") 1202061486L)
[<Fact>]
let ``example 1 falsify`` () =
    let buses = parseInput "data/test_1.txt"
    Assert.False(verifySolution buses 1068780L)
    Assert.False(verifySolution buses 1068782L)

[<Fact>]
let ``parse input`` () =
    let buses = parseInput "data/test_1.txt"
    let busesMatchInput  = (buses = [|(0L,7L,0L);(1L,13L,1L);(4L,59L,4L);(6L,31L,6L);(7L,19L,7L)|])
    Assert.True(busesMatchInput)

let writeInput (input: Input)  =
    for line in input do
        let (o,r,foo) = line 
  //      printfn "x = Bus nr %i departs %i minutes after " r o
 //       printfn "x = whic is really the same as departing %i minutes after " (o % r) 
        printfn "x = %i mod %i" (r - o) r
        ()

let rec sieve cs x N =
    match cs with
    | [] -> Some(x)
    | (a,n)::rest ->
        let arrProgress = Seq.unfold (fun x -> Some(x, x+N)) x
        let firstXmodNequalA = Seq.tryFind (fun x -> a = x % n)
        match firstXmodNequalA (Seq.take n arrProgress) with
        | None -> None
        | Some(x) -> sieve rest x (N*n)

[<EntryPoint>]
let main argv =
    printfn "GOOOO!"
    printfn " "
    writeInput  (parseInput "data/input.txt") 
    printfn " "
    writeInput  (parseInput "data/test_5.txt") 
    printfn " "
    writeInput  (parseInput "data/test_6.txt") 
    // paste input into https://davidwees.com/chineseremaindertheorem/
    0 // return an integer exit code
