﻿open System
open Xunit 

// adapter can 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
// your device has a built-in joltage adapter rated for 3 jolts higher than the 
//highest-rated adapter in your bag

let rating (adapters: int list) =
   List.max adapters + 3 

let fits (adapter: int) (required: int) =
    adapter + 1 = required || adapter + 2 = required || adapter + 3 = required

let chargingOutlet = 0

let findConnectors (adapters: int list) (charger: int) =
    adapters |> List.where (fun a -> fits charger a)    

let pick (adapters: int list) (nextAdapters: int list) : int =
   // smallest one that fits in the nextadapters
   // for each adapter must find the ones that can fit in one... exists one that fits
   let possibleNextAdapters = adapters |> List.map (fun a -> (a, findConnectors nextAdapters a))
   let possibleConnections = possibleNextAdapters 
                             |> List.where ( fun (_,conns) -> not (List.isEmpty conns))
                             |> List.map (fun (a, _) -> a)
   if List.isEmpty possibleConnections then 
       -1
   else 
       List.min possibleConnections 
       
let pickAll (adapters: int list) (nextAdapters: int list) : int list option =
   let possibleNextAdapters = adapters |> List.map (fun a -> (a, findConnectors nextAdapters a))
   let possibleConnections = possibleNextAdapters 
                             |> List.where ( fun (_,conns) -> not (List.isEmpty conns))
                             |> List.map (fun (a, _) -> a)
   if List.isEmpty possibleConnections then 
      None 
   else 
      Some possibleConnections 

let parseFile file = System.IO.File.ReadAllLines file |> Array.map (int) |> Array.toList

let testdata = parseFile "data/foo.txt" 
let longerTestdata = parseFile "data/longerTest.txt" 

let findDifferences (input: int list) (difference: int) = 
    input |> Seq.sort |> Seq.pairwise |> Seq.where (fun (a,b) -> b - a = difference) |> Seq.toList |> List.length 

let rec createVisitedSequence (visited: int list) (input: int list) (start: int) : int list =
    // starter med input legger på til -1... da henter man ratingen og legger til den og
    let possibleConnectors = findConnectors input start 
    let firstConnector = pick possibleConnectors input 
    if firstConnector = -1 then  // little sort of bug where it should probably have returned 19
        let connectTo = possibleConnectors |> List.head
        rating [connectTo ]:: connectTo :: start :: visited 
    else 
        createVisitedSequence (start :: visited) input firstConnector

let createVisitedSequenceforAll (input: int list) (start: int) : int =
    let rec recFunc (nrsFound: int) (start:int) : int = 
        let possibleConnectors = findConnectors input start 
        match possibleConnectors with 
            | [] -> nrsFound + 1
            | [connection] -> nrsFound + (recFunc nrsFound connection)
            | connections -> nrsFound + (connections |> List.sumBy (fun c -> recFunc nrsFound c) )
    recFunc 0 start

let nrOfCombinations (data: int list) = 
    createVisitedSequenceforAll data chargingOutlet 

[<Fact>]
let ``visitAllInLongerTestfile`` () = 
    Assert.Equal(19208, nrOfCombinations longerTestdata)

[<Fact>]
let ``visitAllInTestfile`` () = 
    Assert.Equal(8, nrOfCombinations testdata)

[<Fact>]
let ``visitTestfile`` () = 
    let connectors = createVisitedSequence [] testdata chargingOutlet
    let ones = findDifferences connectors 1
    let threes = findDifferences connectors 3
    Assert.Equal(7, ones)
    Assert.Equal(5, threes)

[<Fact>]
let ``visitLongerTestfile`` () = 
    let connectors = createVisitedSequence [] longerTestdata chargingOutlet
    let ones = findDifferences connectors 1
    let threes = findDifferences connectors 3
    Assert.Equal(22, ones)
    Assert.Equal(10, threes)

let product (inputFile: string) : int =
    let connectors = createVisitedSequence [] (parseFile inputFile) chargingOutlet
    let ones = findDifferences connectors 1
    let threes = findDifferences connectors 3
    ones * threes 

[<Fact>]
let ``visitLongerTestfileProduct`` () = 
    Assert.Equal(220, product "data/LongerTest.txt")

[<Fact>]
let ``part1 is correct`` () = 
    Assert.Equal(2775, product "data/input.txt")
     
[<EntryPoint>]
let main argv =
    printfn "Nr 1 is : %A" (product "data/input.txt")
    printfn "Nr 2 with testdata is : %A" (nrOfCombinations (parseFile "data/longerTest.txt"))
    printfn "Nr 2 is : %A" (nrOfCombinations (parseFile "data/input.txt"))
    0 // return an integer exit code
