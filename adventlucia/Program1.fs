(* 
open System
open Xunit

type Input = int * int [] 

let parseInput (file: string) : Input = 
    let i_raw = IO.File.ReadAllLines file
    let timestamp = int (i_raw.[0]) 
    let line1 = (i_raw.[1]).Split(",")
    let routes = line1 |> Array.where(fun l -> not (l = "x")) |> Array.map (int)
    (timestamp, routes)     

let calcWaittime (interval: int) (timestamp: int) : (int*int) =
    let timeSincePassed = timestamp % interval
    (interval, interval - timeSincePassed)

let calcWaitTimes (input: Input) : (int*int) [] =
    let (timestamp, routes) = input
    routes |> Array.map (fun e -> calcWaittime e timestamp )

let findNextBusline (input: Input) : (int*int) =
    let waitTimes = calcWaitTimes input
    waitTimes |> Array.minBy (fun (_,w) -> w)

[<Fact>]
let ``mintesttime in testdata``() =
    let i = parseInput "data/test_1.txt"
    let (line, waitTime) = findNextBusline i 
    Assert.Equal(5, waitTime) 
    Assert.Equal(59, line) 
    Assert.Equal(295, waitTime * line) 
 
[<Fact>]
let ``mintesttime in inputset``() =
    let i = parseInput "data/input.txt"
    let (line, waitTime) = findNextBusline i 
    Assert.Equal(7, waitTime) 
    Assert.Equal(37, line) 
    Assert.Equal(259, waitTime * line) 
 
[<Fact>]
let ``parse input``() =
    let (timestamp, routes) = parseInput "data/test_1.txt"
    Assert.Equal(939,timestamp)
    Assert.Contains(13, routes)
    let busesMatchInput  = ([|7;13;59;31;19|] = routes)
    Assert.True(busesMatchInput)
    
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
    *)
