module Parser
open Domain
open Xunit
open System.IO

let readInputfile inputfile : string[] =
    File.ReadAllLines inputfile

let parseLine (line: string) : Cmd =
    if line.Contains("acc") then
       let x = int (line.TrimStart("acc ".ToCharArray()))
       Acc x 
    else if line.Contains("jmp") then
       let x = int (line.TrimStart("jmp ".ToCharArray()))
       Jmp x 
    else 
       Nop 

let parseLines (input: string[]): Cmd list =
    input 
      |> Array.map (fun s -> parseLine s) 
      |> Array.toList

[<Fact>]
let ``Parse example file equal lines`` () =
    let file = readInputfile "data/example.txt"
    Assert.Equal(9, parseLines file |> List.length)

[<Fact>]
let ``Parse file equal lines`` () =
    let file = readInputfile "data/input.txt"
    Assert.Equal(file.Length, parseLines file |> List.length)

[<Fact>]
let ``Parse Nop`` () =
    Assert.Equal( Nop, parseLine "nop -4")

[<Fact>]
let ``Parse Jmp 4`` () =
    Assert.Equal( Jmp 4, parseLine "jmp +4")

[<Fact>]
let ``Parse Acc -3`` () =
    Assert.Equal( Acc -3, parseLine "acc -3")

[<Fact>]
let ``Parse Jmp -3`` () =
    Assert.Equal( Jmp -3, parseLine "jmp -3")

[<Fact>]
let ``Read file works`` () =
    Assert.Contains("jmp +5", readInputfile "data/input.txt")


