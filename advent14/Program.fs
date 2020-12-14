open System
open Xunit

open Parser
open Domain

// kanskje minnet kan være en map
type Memory = Map<int64, int64>
type ProgramState = Memory * Program * Bitmask
    
let initialMask = match program with | Mask mask :: _  -> mask 

let initialProgram : ProgramState = (Map.empty, program, initialMask) 

   // make sure this thing is long enough too
let applyBitmask (address: int64) (mask: Bitmask)  : Bitmask =
    let intAsBools = new Collections.BitArray(BitConverter.GetBytes(address)) |> Seq.cast<bool>
    Seq.zip intAsBools mask
        |> Seq.map (fun e -> match e with
                                        | (true,Low) -> Hi 
                                        | (false,Low) -> Low 
                                        | (_,Hi) -> Hi 
                                        | (_,Unchanged) -> Unchanged )
        |> Seq.toArray

[<Fact>]
let ``applyMask to 42`` () =
    let address = 42L;
    let mask = parseMask "000000000000000000000000000000X1001X"
    let result = parseMask "000000000000000000000000000000X1101X"
    let appliedMask = applyBitmask address mask 
    let maskEqualsResult = (result |> Seq.toList) = Seq.toList (appliedMask)
    Assert.True(maskEqualsResult)

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
        | MemInstr (addr, value) :: rest -> let res = evalMemoryInstruction mask value
                                            let mem' = Map.add addr res mem
                                            evaluateInstructions (mem', rest, mask ) // replace bitmask and move on
        | [] ->  (mem, prog, mask) // done

[<Fact>]
let ``last bitmask is in memory`` () = 
    let (mem, prog, mask)= evaluateInstructions initialProgram
    let mask' = parseMask "11X1X01011X1X1X01010000110X0000XX11X"
    let masksMatch = mask = mask'
    Assert.True(masksMatch)

let sumOfMemory (mem : Memory) : int64 =
   mem |> Map.toSeq |> Seq.map snd |> Seq.sum
// instruksjonene 
//The current bitmask is applied to values immediately before they are written to memory
[<EntryPoint>]
let main argv =
    let (mem, prog, mask) = evaluateInstructions initialProgram 
    printfn "%A" (sumOfMemory mem )
    0 // return an integer exit code
