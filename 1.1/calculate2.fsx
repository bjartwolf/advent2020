open System.IO

let firstNumbers = File.ReadLines("input.txt") |> Seq.toList |> List.map int
let secondNumber = firstNumbers
let thirdNumber = firstNumbers

let sumFirstNumbes (x:int) = secondNumber 
                                |> List.map (fun y -> (x,y)) 

let firstCalculation = List.collect (sumFirstNumbes) firstNumbers

let sumSecondNumbers (z:int) = firstCalculation 
                                |> List.map (fun (x,y) -> (x,y,z,x+y+z)) 

let allCalculations = List.collect (sumSecondNumbers) thirdNumber 
printf "%A" (List.last allCalculations)

let twentytwenty = allCalculations 
                        |> List.filter (fun (x,y,z,sum) -> sum = 2020)
                        |> List.map (fun (x,y,z,sum) -> (x,y,z,x*y*z))

printf "%A" twentytwenty 
