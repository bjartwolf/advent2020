open Domain
open Parser 
open Xunit

[<Fact>]
let ``Test data completes in order`` () =
    let programTxt = readInputfile "data/modifiedExampleInOrder.txt" 
    let program = parseLines programTxt

    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.StrictEqual( [8;7;6;2;1;0], result.Visited)
    Assert.Equal(8, result.Acc)
    Assert.Equal(programTxt.Length - 1, List.head result.Visited)

[<Fact>]
let ``Test data with jmp0 completes`` () =
    let programTxt = readInputfile "data/exampleWithJmp0.txt" 
    let program = parseLines programTxt

    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.StrictEqual( [0], result.Visited)
    Assert.Equal(0, result.Acc)

[<Fact>]
let ``Test data completes`` () =
    let programTxt = readInputfile "data/example.txt" 
    let program = parseLines programTxt

    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.StrictEqual( [4;3;7;6;2;1;0], result.Visited)
    Assert.Equal(5, result.Acc)

[<Fact>]
let ``Code first part as test`` () =
    let programTxt = readInputfile "data/input.txt" 
    let program = parseLines programTxt

    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.Equal(1782, result.Acc)

[<Fact>]
let ``Unmodified input does not terminate "normally"`` () =
    let programTxt = readInputfile "data/input.txt" 
    let program = parseLines programTxt
    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.Equal(1782, result.Acc)
    Assert.NotEqual(programTxt.Length - 1, List.head result.Visited)

[<Fact>]
let ``Fixed input does terminate "normally"`` () =
    let programTxt = readInputfile "data/inputFixed.txt" 
    let program = parseLines programTxt
    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.Equal(programTxt.Length - 1, List.head result.Visited)

[<Fact>]
let ``Hack input until it terminates "normally"`` () =
    
    let findIndexToSwap : int = 
        let programTxt = readInputfile "data/input.txt" 
        let mutable index = -1
        for i in 0 .. (programTxt.Length - 1) do 
            let programTxt = readInputfile "data/input.txt" 
            let line = programTxt.[i]
            if line.Contains("jmp") then
                programTxt.[i] <- line.Replace("jmp", "nop")
            else if line.Contains("nop") then
                programTxt.[i] <- line.Replace("nop", "jmp")
            let program = parseLines programTxt
            let result = evaluateProgram program
            if (programTxt.Length - 1 = (List.head result.Visited)) then
                index <- i    
        index 
                
    let index = findIndexToSwap 
    Assert.Equal(253, index)

[<EntryPoint>]
let main argv =
    let file = readInputfile "data/input.txt"
    let program = parseLines file
    let result = evaluateProgram program
    printf "%A" result 
    0
