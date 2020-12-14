﻿module Parser 
open System
open Xunit

type Op = Hi | Low | Unchanged
type Bitmask = Op array 
let testmask = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

type LoadToMem = int * int
type Instruction = Mask of Bitmask | MemInstr of LoadToMem 
type Program = Instruction list


let parseMemory (input: string) : LoadToMem =
    let s = input.Split("=")
    let x = (s.[0]).Replace("mem[", "").Replace("]", "")
    (int x, int s.[1])

[<Fact>]
let ``parse memory works`` () = 
   Assert.Equal((414, 4313357), parseMemory "mem[414] = 4313357")
   Assert.Equal((666,32), parseMemory "mem[666] = 32")

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

    [ Mask (parseMask programCode.[0]);
      MemInstr (parseMemory programCode.[1]) ]

[<Fact>]
let ``parse testprogram works`` () =
    let program = parseProgram "input.txt" 
    let firstInstr = program.Head
    let secondInst = program.Tail.Head
    match firstInstr with | Mask mask -> Assert.Equal(Hi, mask.[35])
    match secondInst with | MemInstr i-> Assert.Equal((6540, 1053547115), i) 
    
[<Fact>]
let ``parse testmask works`` () = 
   let mask = parseMask testmask
   Assert.Equal(Unchanged, mask.[35]) 
   Assert.Equal(Unchanged, mask.[0]) 
   Assert.Equal(Low, mask.[1]) 
   Assert.Equal(Hi, mask.[6]) 

