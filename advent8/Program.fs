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

[<EntryPoint>]
let main argv =
    let file = readInputfile "data/input.txt"
    let program = parseLines file
    let result = evaluateProgram program
    printf "%A" result 
    0
