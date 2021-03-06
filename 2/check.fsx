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

type RegexAndPassword = { rule: Regex;
                          password: string}

let parseLine (line:string) : RulesAndPassword =
    let res = line.Split [|' '; ':'; '-'|] 
    { min = int res.[0];
      max = int res.[1];
      character = res.[2].[0];
      password = res.[4]}

let makeRegex rulesAndPassword : RegexAndPassword =
    let str = sprintf @"^(?:[^%O]*%O){%i,%i}[^%O]*$" rulesAndPassword.character
                                                     rulesAndPassword.character
                                                     rulesAndPassword.min
                                                     rulesAndPassword.max
                                                     rulesAndPassword.character
    { rule = Regex str;
      password = rulesAndPassword.password } 

readLines "passord.txt" 
    |> Seq.map parseLine 
    |> Seq.map makeRegex
    |> Seq.filter (fun i -> (i.rule.IsMatch (i.password  |> Seq.sort |> String.Concat)))
    //|> Seq.map (fun i -> i.rule.IsMatch (i.password  |> Seq.sort |> String.Concat), i)
    |> Seq.toList
    |> List.length
    |> printf "%A \n\n"  
//    |> List.length
//    |> printf "%A \n"