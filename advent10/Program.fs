open System
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
                             |> List.where ( fun (a,conns) -> not (List.isEmpty conns))
                             |> List.map (fun (a, conns) -> a)
   if List.isEmpty possibleConnections then 
       -1
   else 
       List.min possibleConnections 

let testdata = System.IO.File.ReadAllLines "data/foo.txt" |> Array.map (int) |> Array.toList

[<Fact>]
let ``testdata adapters only 4 can connect to 1`` () = 
    let firstConnectors = findConnectors testdata chargingOutlet 
    let firstConnector = pick firstConnectors testdata
    Assert.Equal(1, firstConnector) 

    let secondConnectors = findConnectors testdata firstConnector 
    Assert.Contains(4, secondConnectors)
    let secondConnector = pick secondConnectors testdata
    Assert.Equal(4, secondConnector)

    let thirdConnectors = findConnectors testdata secondConnector
    Assert.Contains(5, thirdConnectors)
    Assert.Contains(6, thirdConnectors)
    Assert.Contains(7, thirdConnectors)
    let thirdConnector = pick thirdConnectors testdata
    Assert.Equal(5, thirdConnector) 

    let fourthConnectors = findConnectors testdata thirdConnector 
    Assert.Contains(6, fourthConnectors)
    Assert.Contains(7, fourthConnectors)
    let fourthConnector = pick fourthConnectors testdata
    Assert.Equal(6, fourthConnector) 

    let fifthConnectors = findConnectors testdata fourthConnector 
    Assert.Contains(7, fifthConnectors)
    let fifthConnector = pick fifthConnectors testdata
    Assert.Equal(7, fifthConnector) 

    let sixthConnectors = findConnectors testdata fifthConnector 
    Assert.Contains(10, sixthConnectors)
    Assert.Equal(1, sixthConnectors.Length)
    let sixthConnector = pick sixthConnectors testdata
    Assert.Equal(10, sixthConnector) 

    let seventhConnectors = findConnectors testdata sixthConnector 
    Assert.Contains(11, seventhConnectors)
    Assert.Contains(12, seventhConnectors)
    Assert.Equal(2, seventhConnectors.Length)
    let sevenConnector = pick seventhConnectors testdata
    Assert.Equal(11, sevenConnector)

    let eigthConnectors = findConnectors testdata sevenConnector 
    Assert.Contains(12, eigthConnectors) 
    let eigthConnector = pick eigthConnectors testdata
    Assert.Equal(12, eigthConnector)

    let nines = findConnectors testdata eigthConnector 
    Assert.Contains(15, nines) 
    let nine = pick nines testdata
    Assert.Equal(15, nine)

    let tens = findConnectors testdata nine 
    Assert.Contains(16, tens) 
    let ten = pick tens testdata
    Assert.Equal(16, ten)

    let elevens = findConnectors testdata ten 
    Assert.Contains(19, elevens) 
    let eleven = pick elevens testdata
    Assert.Equal(-1, eleven)
    Assert.Equal(22, rating elevens)

[<Fact>]
let ``testdata adaptes rates to 22`` () = 
    Assert.Equal(22, rating testdata)

[<Fact>]
let ``3 6 and 9 rates adapter to 12`` () = 
    let adapters = [3;6;9] 
    Assert.Equal(12, rating adapters)
    Assert.True(fits 9 12)
    Assert.True(fits 8 11)
    Assert.False(fits 8 12)
    Assert.False(fits 12 12)
    Assert.False(fits 14 12)
    Assert.False(fits 1 12)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
