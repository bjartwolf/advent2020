open System.IO

let firstNumbers = File.ReadLines("input.txt") |> Seq.toList |> List.map int
let secondNumber = firstNumbers

let sumAllNumbers (x:int) = secondNumber |> List.map (fun y -> (x,y,x+y)) 
let allCalculations = firstNumbers |> List.map (fun x -> sumAllNumbers x) |> List.concat
let twentytwenty = allCalculations |> List.filter (fun (x,y,z) -> z = 2020) |> List.map (fun (x,y,z) -> (x,y,x*y))
printf "%A" twentytwenty 
