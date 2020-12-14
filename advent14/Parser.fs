module Parser 

open System
open Xunit
open Domain

let testmask = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

let parseMemory (input: string) : LoadToMem =
    let s = input.Split("=")
    let x = (s.[0]).Replace("mem[", "").Replace("]", "")
    (int64 x, int64 s.[1])

[<Fact>]
let ``parse memory works`` () = 
   Assert.Equal((414L, 4313357L), parseMemory "mem[414] = 4313357")
   Assert.Equal((666L,32L), parseMemory "mem[666] = 32")

let parseMask (input:string) : Bitmask =
    let mutable array = Array.create 36 Unchanged 
    input.Replace("mask = ", "")
        |> Seq.rev 
        |> Seq.mapi (fun i -> fun e -> 
                                match e with 
                                    | 'X' -> array.[i] <- Unchanged
                                    | '0' -> array.[i] <- Low 
                                    | '1' -> array.[i] <- Hi ) |> Seq.toList |> ignore
    // should do something else
    // this requires the sequnece to be evaluated, should be something do for each, but
    // i would like the iterator like a for each. but it does not really matter
    array

let parseProgram inputFile : Program =
    let programCode = IO.File.ReadAllLines inputFile
    let parsedLines = seq { 
        for line in programCode do
            if line.Contains("mask") then 
                yield Mask (parseMask line)         
            else 
                yield MemInstr (parseMemory line)
    }
    parsedLines |> Seq.toList

let program = parseProgram "input.txt" 

[<Fact>]
let ``parse testprogram works`` () =
    let firstInstr = program.Head
    let secondInst = program.Tail.Head
    let thirdInstr = program |> List.skip 3 |> List.head
    match firstInstr with | Mask mask -> Assert.Equal(Hi, mask.[35])
    match secondInst with | MemInstr i-> Assert.Equal((6540L, 1053547115L), i) 
    match thirdInstr with | MemInstr (13014L,7128L)-> Assert.True(true)
    
[<Fact>]
let ``parse testmask works`` () = 
   let mask = parseMask testmask
   Assert.Equal(Unchanged, mask.[35]) 
   Assert.Equal(Unchanged, mask.[0]) 
   Assert.Equal(Low, mask.[1]) 
   Assert.Equal(Hi, mask.[6]) 

