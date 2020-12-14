open System
open Xunit

open Parser
open Domain

// kanskje minnet kan være en map
type Memory = Map<int64, int64>
type ProgramState = Memory * Program * Bitmask
    
let initialMask = match program with | Mask mask :: _  -> mask 

let initialProgram : ProgramState = (Map.empty, program, initialMask) 

let boolsToInt (bools: bool seq) : int64 =
    bools |> Seq.mapi (fun i -> fun e -> if e then pown 2L i else 0L) |> Seq.sum

let bitmaskToInt (b: Bitmask) : int64 =
    b |> Seq.map (fun e -> match e with
                                        | Hi -> true
                                        | Low -> false)
      |> boolsToInt 

let evalMemoryInstruction (mask: Bitmask) (memValue: int64) : int64 =
    let intAsBytes = new Collections.BitArray(BitConverter.GetBytes(memValue)) |> Seq.cast<bool>
    Seq.zip intAsBytes mask
        |> Seq.map (fun e -> match e with
                                        | (_,Hi) -> true
                                        | (_,Low) -> false
                                        | (b,Unchanged) -> b) 
        |> boolsToInt

// make sure this thing is long enough in case things are not long enough
// looks like it works... maybe because the int64 always are longer than the masks
// also a bit hacky reusing the bitmask as a new bitmask, or maybe not.
let applyBitmask (address: int64) (mask: Bitmask)  : Bitmask =
    let intAsBools = new Collections.BitArray(BitConverter.GetBytes(address)) |> Seq.cast<bool>
    Seq.zip intAsBools mask
        |> Seq.map (fun e -> match e with
                                        | (true,Low) -> Hi 
                                        | (false,Low) -> Low 
                                        | (_,Hi) -> Hi 
                                        | (_,Unchanged) -> Unchanged )
        |> Seq.toList

// could of course make a sequnce a not a list, but not sure it helps me much
let rec generateCombinations (mask: Bitmask) : Bitmask seq =
    seq {
        if (mask |> Seq.contains (Unchanged)) then 
            let foo = mask |> List.toArray
            let firstX = foo |> Array.findIndex (fun f -> f = Unchanged) 
            foo.[firstX] <- Hi
            yield! generateCombinations (foo |> Array.toList)
            foo.[firstX] <- Low 
            yield! generateCombinations (foo |> Array.toList)
        else
            yield mask
    }

let funkyApplication (address: int64) (mask: Bitmask) : int64 list =
    let mask' = applyBitmask address mask
    let combinations = generateCombinations mask'
    combinations |> Seq.map (bitmaskToInt) |> Seq.toList

[<Fact>]
let ``applyMask to 42 and yield the results`` () =
    let address = 42L;
    let mask = parseMask "000000000000000000000000000000X1001X"
    let ints = funkyApplication address mask 
    Assert.Equal(4, ints |> Seq.length)

    Assert.Contains(26L, ints )
    Assert.Contains(27L, ints)
    Assert.Contains(58L, ints)
    Assert.Contains(59L, ints)

[<Fact>]
let ``applyMask to 26 and yield the results`` () =
    let address = 26L;
    let mask = parseMask "00000000000000000000000000000000X0XX"
    let ints = funkyApplication address mask 
    Assert.Equal(8, ints |> Seq.length)

    Assert.Contains(16L, ints )
    Assert.Contains(17L, ints)
    Assert.Contains(18L, ints)
    Assert.Contains(19L, ints)
    Assert.Contains(24L, ints)
    Assert.Contains(25L, ints)
    Assert.Contains(26L, ints)
    Assert.Contains(27L, ints)

    
[<Fact>]
let ``applyMask to 42`` () =
    let address = 42L;
    let mask = parseMask "000000000000000000000000000000X1001X"
    let result = parseMask "000000000000000000000000000000X1101X"
    let appliedMask = applyBitmask address mask 
    let maskEqualsResult = (result |> Seq.toList) = Seq.toList (appliedMask)
    Assert.True(maskEqualsResult)

   
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

let rec updateMapWithManyAddresses (map: Map<int64,int64>) (addresses: int64 list) (res: int64) : Map<int64,int64> =
    match addresses with
        | [] -> map
        | address :: rest -> let m' = Map.add address res map
                             updateMapWithManyAddresses m' rest res  

let rec evaluateInstructionsFunky (mem, prog, mask) : ProgramState =
    match prog with 
        | Mask bitmask :: rest ->  evaluateInstructionsFunky (mem, rest, bitmask) // replace bitmask and move on
        | MemInstr (addr, value) :: rest -> let addresses = funkyApplication addr mask
                                            let mem' = updateMapWithManyAddresses mem addresses value 
                                            evaluateInstructionsFunky (mem', rest, mask ) // replace bitmask and move on
        | [] ->  (mem, prog, mask) // done

let sumOfMemory (mem : Memory) : int64 =
   mem |> Map.toSeq |> Seq.map snd |> Seq.sum

[<Fact>]
let ``Funkycode works`` () = 
    let initialMask = parseMask "000000000000000000000000000000X1001X";
    let program : Program = [ Mask (parseMask "000000000000000000000000000000X1001X");
                              MemInstr (parseMemory "mem[42] = 100");
                              Mask (parseMask "00000000000000000000000000000000X0XX");
                              MemInstr (parseMemory "mem[26] = 1"); ]
    let initialProgram = (Map.empty, program, initialMask) 
    let (memfunky, prog, mask) = evaluateInstructionsFunky initialProgram 
    Assert.Equal(208L, sumOfMemory memfunky )
    

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
    let (mem, prog, mask) = evaluateInstructions initialProgram 
    let (memfunky, prog, mask) = evaluateInstructionsFunky initialProgram 
    printfn "%A" (sumOfMemory mem )
    printfn "Funkyres %A" (sumOfMemory memfunky )
    0 // return an integer exit code
