open Domain
open Parser 
open Xunit

[<Fact>]
let ``Test data completes`` () =
    let programTxt = readInputfile "data/example.txt" 
    let program = parseLines programTxt

    let result = evaluateProgram program
    Assert.True(isCompleted program result)
    Assert.Equal(5, result.Acc)


[<EntryPoint>]
let main argv =
    let file = readInputfile "data/input.txt"
    let program = parseLines file
    let result = evaluateProgram program
    printf "%A" result 
    0
