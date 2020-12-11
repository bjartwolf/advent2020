open System
open System.Collections.Generic
open Xunit 

let rating (adapters: int list) =
   List.max adapters + 3 

let fits (adapter: int) (required: int) =
    adapter + 1 = required || adapter + 2 = required || adapter + 3 = required

let chargingOutlet = 0

let findConnectors (adapters: int list) (charger: int) =
    adapters |> List.where (fun a -> fits charger a)    

let pick (adapters: int list) (nextAdapters: int list) : int =
   let possibleNextAdapters = adapters |> List.map (fun a -> (a, findConnectors nextAdapters a))
   let possibleConnections = possibleNextAdapters 
                             |> List.where ( fun (_,conns) -> not (List.isEmpty conns))
                             |> List.map (fun (a, _) -> a)
   if List.isEmpty possibleConnections then 
       -1
   else 
       List.min possibleConnections 
       
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

<<<<<<< HEAD
let rec arr (current : int ) (cache : int64 option array) : int64 =
    if current >= Array.length cache then
        0L
    else
        match cache.[current] with 
        | Some c -> c
        | None ->
            let c = [1..3] |> List.sumBy (fun i -> arr (current + i) cache)
            cache.[current] <- Some c
            c

let createVisitedSequenceforAll (input: int list) (start: int) (max: int): int64 =
    let maxRating = rating input
    let input' = chargingOutlet :: maxRating :: input |> List.sort
    let foo = [| for i in 0 .. maxRating -> if List.contains i input' then None else Some 0L |]
    foo.[maxRating] <- Some 1L 
    arr 0 foo 
=======

let createVisitedSequenceforAll (input: int list) (start: int) : int64 =
    let cache = Dictionary<int, int64>()
    let cachedFunc f x = 
        let incache, value = cache.TryGetValue(x)
        if (incache) then
            value
        else 
            let res = f x 
            cache.Add(x,res)
            res

    let rec recFunc (start:int) : int64 = 
       let possibleConnectors = findConnectors input start 
       match possibleConnectors with 
            | [] -> 1L
            | [a] -> (cachedFunc recFunc) a
            | a :: b :: [] -> (cachedFunc recFunc) a + (cachedFunc recFunc) b
            | a :: b :: c :: [] -> (cachedFunc recFunc) a + (cachedFunc recFunc) b + (cachedFunc recFunc) c
    recFunc start

let nrOfCombinations (data: int list) = 
    createVisitedSequenceforAll data chargingOutlet 

[<Fact>]
let ``visitAllInLongerTestfile`` () = 
   Assert.Equal(19208L, nrOfCombinations longerTestdata)

[<Fact>]
let ``visitAllInTestfile`` () = 
    Assert.Equal(8L, nrOfCombinations testdata)

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
let ``visitLongerTestfileProductSmall`` () = 
    Assert.Equal(35, product "data/foo.txt")

[<Fact>]
let ``visitLongerTestfileProduct`` () = 
    Assert.Equal(220, product "data/LongerTest.txt")

[<Fact>]
let ``part1 is correct`` () = 
    Assert.Equal(2775, product "data/input.txt")

[<Fact>]
let ``part2 is correct`` () = 
    Assert.Equal(518344341716992L , nrOfCombinations (parseFile "data/input.txt"))
      
[<EntryPoint>]
let main argv =
    printfn "Nr 1 is : %A" (product "data/input.txt")
    printfn "Nr 2 latest with testdata is : %A" (nrOfCombinations (parseFile "data/longerTest.txt"))
    printfn "Nr 2 is... : %A" (nrOfCombinations (parseFile "data/input.txt") 2775)
    0 // return an integer exit code
