// Learn more about F# at http://fsharp.org

open System
open Xunit

// define mycosine and my sine?
type Dir = int

type Pos = int * int
type WP =  Pos 

type Boat = { p: Pos;
              wp: WP }

type Cmd =  N of int | S of int | E of int | W of int 
            | L of int | R of int | F of int

let readCommands filename = 
    IO.File.ReadAllLines filename 

let parseCmd (line: string) : Cmd =
    let pss (s:string) : int =
        int (line.Substring(1))
    if line.StartsWith("N") then
        N (pss line)
    else if line.StartsWith ("S") then
        S (pss line) 
    else if line.StartsWith ("E") then
        E (pss line) 
    else if line.StartsWith ("W") then
        W (pss line) 
    else if line.StartsWith ("L") then
        L (pss line) 
    else if line.StartsWith ("R") then
        R (pss line) 
    else if line.StartsWith ("F") then
        F (pss line) 
    else
        failwith "bad input data or horrible parser"

let parseCommands (input: string array) : Cmd array =
    input |> Array.map (parseCmd) 

let getCommands (i: string) : Cmd array =
    i |> readCommands |> parseCommands 

let translateWp (wp: WP) (distX: int) (distY: int)  : WP =
    let (x,y) = wp 
    (x+distX, y+distY)

let intSin (degrees: int) : int = 
    let angle = Math.PI * (double degrees) / 180.0;
    int (Math.Sin(angle))

let intCos (degrees: int) : int = 
    let angle = Math.PI * (double degrees) / 180.0;
    int (Math.Cos(angle))

let rotate (wp: WP) (deg: int) : WP = 
    let (x,y) = wp
    let x' = x*(intCos deg) - y*(intSin deg)
    let y' = x*(intSin deg) + y*(intCos deg)
    (x', y')

let rotateLeft (b: Boat) (degrees: int) : Boat =
    { b with wp = rotate b.wp degrees }

let rotateRight (b: Boat) (degrees: int) : Boat =
    { b with wp  = rotate b.wp -degrees }

let moveNorth (dist: int) (b: Boat)  : Boat = 
    { b with wp = translateWp b.wp 0 dist }
let moveEast (dist: int) (b: Boat ) : Boat = 
    { b with wp = translateWp b.wp dist 0 }
let moveSouth (dist: int) (b: Boat ) : Boat = 
    { b with wp = translateWp b.wp 0 -dist }
let moveWest (dist: int) (b: Boat ) : Boat = 
    { b with wp = translateWp b.wp -dist 0 }

let moveForward (dist: int) (b: Boat ) : Boat =
    let (x,y) = b.p
    let (wpx, wpy) = b.wp
    let x' = x + dist * wpx 
    let y' = y + dist * wpy 
    { b with p = (x', y')}

let execute (c: Cmd) (b: Boat ) : Boat =
    match c with 
        | N dist -> moveNorth dist b
        | E dist -> moveEast dist b
        | W dist -> moveWest dist b
        | S dist -> moveSouth dist b
        | L deg -> rotateLeft b deg
        | R deg -> rotateRight b deg
        | F dist -> moveForward dist b 

//need a small refactor.

let rec executeCommands (cmds: Cmd list) (b: Boat) : Boat=
    match cmds with
        | c :: rest -> executeCommands rest (execute c b)
        | [] -> b    

let taxiCabFromOrigin (b:Boat) : int = 
    let (x,y) = b.p
    Math.Abs x + Math.Abs y

[<Fact>]
let ``Trig works`` () =
    Assert.Equal(1, intCos 0) 
    Assert.Equal(0, intCos 90) 
    Assert.Equal(-1, intCos 180) 
    Assert.Equal(-1, intCos -180) 
    Assert.Equal(0, intCos 270) 
    Assert.Equal(0, intCos -270) 
    Assert.Equal(1, intCos 360) 
    Assert.Equal(1, intCos -360) 
    Assert.Equal(0, intSin 0) 

let initialWp = (10,1)
let initialBoat = { p= (0,0);
                    wp = (10,1) }


[<Fact>]
let ``Boat moves correct with testdata`` () =
    let cmds = getCommands "data/test1.txt" |> Array.toList
    let b' = executeCommands cmds initialBoat
    let (x',y') = b'.p
    Assert.Equal(214, x')
    Assert.Equal(-72, y')
    Assert.Equal(286, taxiCabFromOrigin b')

[<Fact>]
let ``Boat moves correct with inputdata`` () =
    let cmds = getCommands "data/input.txt" |> Array.toList
    let b' = executeCommands cmds initialBoat
    Assert.Equal(50157, taxiCabFromOrigin b')
  
[<Fact>]
let ``Commands are parsed`` () =
    let cmds = getCommands "data/test1.txt"
    let answer : Cmd array = [| F 10;
                                N 3;
                                F 7;
                                R 90;
                                F 11;|] 
    Assert.True((answer = cmds))
    Assert.Equal(5, cmds.Length)


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
