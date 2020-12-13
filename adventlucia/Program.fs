﻿open System
open Xunit

// Route and offset, makes int64 to make it easier to compare 
type Bus = int64 * int64 
type Input = Bus [] 

let parseInput (file: string) : Input = 
    let i_raw = IO.File.ReadAllLines file
    let line1 = (i_raw.[1]).Split(",")
    line1 |> Array.mapi (fun o -> fun s -> (o,s))
          |> Array.where(fun (_,s) -> not (s = "x"))
          |> Array.map (fun (o,r) -> int64 r, int64 o)

let verifySolution (i: Input) (ts: int64) : bool =
    i |> Array.forall (fun (r,o) -> (ts + o) % r = 0L)

let solutions (i: Input) = 
    let (r,_) = i.[0]
    seq {
       for ts in 0L .. r .. Int64.MaxValue do if (verifySolution i ts) then ts  
    }

[<Fact>]
let ``examples`` () =
    Assert.True(verifySolution (parseInput "data/test_1.txt") 1068781L)
    Assert.Equal(solutions (parseInput "data/test_1.txt") |> Seq.head, 1068781L)
//    Assert.Equal(solutions (parseInput "data/input.txt") |> Seq.head, 1068781L)
    Assert.True(verifySolution (parseInput "data/test_2.txt") 3417L)
    Assert.True(verifySolution (parseInput "data/test_3.txt") 754018L)
    Assert.True(verifySolution (parseInput "data/test_4.txt") 779210L)
    Assert.Equal(solutions (parseInput "data/test_4.txt") |> Seq.head, 779210L)
    Assert.True(verifySolution (parseInput "data/test_5.txt") 1261476L)
    Assert.Equal(solutions (parseInput "data/test_5.txt") |> Seq.head, 1261476L)
    Assert.True(verifySolution (parseInput "data/test_6.txt") 1202161486L)
    Assert.Equal(solutions (parseInput "data/test_6.txt") |> Seq.head, 1202161486L)

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
    let busesMatchInput  = (buses = [|(7L,0L);(13L,1L);(59L,4L);(31L,6L);(19L,7L)|])
    Assert.True(busesMatchInput)
    
[<EntryPoint>]
let main argv =
    let solution = solutions (parseInput "data/input.txt") |> Seq.head
    printfn "%A" solution 
    0 // return an integer exit code
