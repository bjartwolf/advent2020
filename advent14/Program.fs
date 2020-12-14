open System
open Xunit

open Parser
open Domain

// kanskje minnet kan være en map
type Memory = Map<int64, int64>
type ProgramState = Memory * Program * Bitmask
    
let initialMask = match program with | Mask mask :: _  -> mask 

let initialProgram : ProgramState = (Map.empty, program, initialMask) 

let evalMemoryInstruction (mask: Bitmask) (memValue: int64) : int64 =
    let intAsBytes = new Collections.BitArray(BitConverter.GetBytes(memValue)) |> Seq.cast<bool>
    Seq.zip intAsBytes mask
        |> Seq.map (fun e -> match e with
                                        | (_,Hi) -> true
                                        | (_,Low) -> false
                                        | (b,Unchanged) -> b) 
        |> Seq.mapi (fun i -> fun e -> if e then pown 2L i else 0L) 
        |> Seq.sum
    
[<Fact>]
let ``Evaluate an instruction`` () =
    let mask = parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    Assert.Equal(73L, evalMemoryInstruction mask 11L)
    Assert.Equal(101L, evalMemoryInstruction mask 101L)
    Assert.Equal(64L, evalMemoryInstruction mask 0L)

let rec evaluateInstructions (mem, prog, mask) : ProgramState =
    match prog with 
        | Mask bitmask :: rest ->  evaluateInstructions (mem, rest, bitmask) // replace bitmask and move on
        | MemInstr instr :: rest ->  evaluateInstructions (mem, rest, mask ) // replace bitmask and move on
        | [] ->  (mem, prog, mask) // done

[<Fact>]
let ``last bitmask is in memory`` () = 
    let (mem, prog, mask)= evaluateInstructions initialProgram
    let mask' = parseMask "11X1X01011X1X1X01010000110X0000XX11X"
    let masksMatch = mask = mask'
    Assert.True(masksMatch)

// instruksjonene 
//The current bitmask is applied to values immediately before they are written to memory
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
