open System.IO
open System
open System.Text.RegularExpressions

// File.ReadLines relies on enumerations finally block to dispose stream 
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
type RulesAndPassword = { min : int;
                          max: int; 
                          character: char;
                          password: string}

let parseLine (line:string) : RulesAndPassword =
    let res = line.Split [|' '; ':'; '-'|] 
    { min = int res.[0] - 1;
      max = int res.[1] - 1;
      character = res.[2].[0];
      password = res.[4]}

let isValid i: bool =
    let pos1 = i.password.Length >= i.min &&
               i.password.[i.min] = i.character
    let pos2 = i.password.Length >= i.max &&
               i.password.[i.max] = i.character
    (pos1 || pos2) && not (pos1 && pos2)

readLines "passord.txt" 
    |> Seq.map parseLine 
    |> Seq.filter isValid 
    |> Seq.toList
    |> List.length
    |> printf "%A \n\n"  