open System
open Xunit

type Point = int * int * int * int
type Points = Set<Point> 

let add ((x1,y1,z1,t1): Point) ((x2,y2,z2,t2) : Point) : Point = (x1+x2, y1+y2, z1+z2, t1 + t2)

let neighbors (p:Point) : Points =  
    let diff = [-1;0;1]; 
    seq { 
        for x in diff do
            for y in diff do
                for z in diff do
                    for t in diff do
                        if (x,y,z,t) <> (0,0,0,0) then 
                            yield (add p (x,y,z,t))
    } |> Set.ofSeq

// returns a set of possible cells that might be alive
let findCandidates (s:Points) : Points = 
    s |>  Set.map (neighbors) |> Set.unionMany

let countAliveNeighbors (p:Point) (s: Points): int =
    let n = neighbors p
    let aliveNeighbors = n |> Set.intersect s
    aliveNeighbors |> Seq.length

let nextGeneration (s: Points) : Points = 
    let candidates = findCandidates s
    candidates |> Set.filter ( fun c -> let isAlive = Set.contains c s  
                                        let aliveNeighbors = countAliveNeighbors c s 
                                        if (isAlive && (aliveNeighbors = 2|| aliveNeighbors = 3)) then true
                                        else if (not isAlive && (aliveNeighbors = 3)) then true
                                        else false )

let inputData : Points =
    let mutable i = 0
    let mutable j = 0
    let points = seq {
        for line in IO.File.ReadAllLines("data.txt") do
            j <- 0
            i <- i + 1 
            for char in line do
                j <- j + 1
                if (char = '#') then yield (i,j,0,0)
    }
    points |> Set.ofSeq
    

[<Fact>]
let ``find alive neighbors`` () =
    let n = countAliveNeighbors (10,1,2,0) (Set.empty)
    Assert.Equal(0, n)

let countPlane (s: Points) (plane: int) : int =
    s |> Set.filter (fun (_,_,p,0) -> p = plane) |> Set.count

[<Fact>]
let ``input 6 rounds is 120 alive`` () =
    let gen0: Points = inputData 
    let gen1 = nextGeneration gen0
    let gen2 = nextGeneration gen1
    let gen3 = nextGeneration gen2
    let gen4 = nextGeneration gen3
    let gen5 = nextGeneration gen4
    let gen6 = nextGeneration gen5
    Assert.Equal(247, gen6 |> Set.count)

[<Fact>]
let ``test 6 rounds is 120 alive`` () =
    let gen0: Points = Set.ofList[ (0,1,0,0); (1,2,0,0); (2,0,0,0); (2,1,0,0); (2,2,0,0) ] 
    let gen1 = nextGeneration gen0
    let gen2 = nextGeneration gen1
    let gen3 = nextGeneration gen2
    let gen4 = nextGeneration gen3
    let gen5 = nextGeneration gen4
    let gen6 = nextGeneration gen5
    Assert.Equal(112, gen6 |> Set.count)

[<Fact>]
let ``test1`` () =
    let gen0: Points = Set.ofList[ (0,1,0,0); (1,2,0,0); (2,0,0,0); (2,1,0,0); (2,2,0,0) ] 
    let gen1 = nextGeneration gen0 
    Assert.Equal(5, countPlane gen1 0) 
    Assert.Equal(3, countPlane gen1 1) 
    Assert.Equal(3, countPlane gen1 -1) 

    let gen2 = nextGeneration gen1 
    Assert.Equal(1, countPlane gen2 -2) 
    Assert.Equal(5, countPlane gen2 -1) 
    Assert.Equal(9, countPlane gen2 0) 
    Assert.Equal(5, countPlane gen2 1) 
    Assert.Equal(1, countPlane gen2 2) 

    let gen3 = nextGeneration gen2 
    Assert.Equal(5, countPlane gen3 -2) 
    Assert.Equal(10, countPlane gen3 -1) 

[<Fact>]
let ``find alive neighbors with itself in the set`` () =
    let s = Set.ofList[ (10,1,2,0); (10,1,3,0); (10,1,4,0)]
    let n = countAliveNeighbors (10,1,2,0) s 
    Assert.Equal(1, n) 

[<Fact>]
let ``find neighbors`` () =
    let n = neighbors (10,1,2,0)
    Assert.Equal(26, n |> Seq.length)

[<EntryPoint>]
let main argv =
    let gen0: Points = inputData 
    let gen1 = nextGeneration gen0
    let gen2 = nextGeneration gen1
    let gen3 = nextGeneration gen2
    let gen4 = nextGeneration gen3
    let gen5 = nextGeneration gen4
    let gen6 = nextGeneration gen5
    printfn "Answer if %A" (gen6 |> Set.count)
    0 // return an integer exit code
