﻿// Learn more about F# at http://fsharp.org

open System
open Xunit

// define mycosine and my sine?
type Dir = int

type Pos = int * int
type WP = { dir: Dir;
            pos: Pos }

type Boat = { b: Pos;
              d: Dir;
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

let translate (b: WP) (distX: int) (distY: int)  : WP =
    let (x,y) = b.pos 
    { b with pos = (x+distX, y+distY)}

let rotateLeft (b: WP) (degrees: int) : WP =
    let dir' = b.dir + degrees 
    { b with dir = dir' }

let rotateRight (b: WP) (degrees: int) : WP =
    let dir' = b.dir - degrees 
    { b with dir = dir' }

let moveNorth (dist: int) (b: WP) : WP = translate b 0 dist
let moveEast (dist: int) (b: WP ) : WP = translate b dist 0
let moveSouth (dist: int) (b: WP ) : WP = translate b 0 -dist
let moveWest (dist: int) (b: WP ) : WP = translate b -dist 0

let intSin (degrees: int) : int = 
    let angle = Math.PI * (double degrees) / 180.0;
    int (Math.Sin(angle))

let intCos (degrees: int) : int = 
    let angle = Math.PI * (double degrees) / 180.0;
    int (Math.Cos(angle))

let moveForward (dist: int) (b: WP ) : WP =
    let (x,y) = b.pos
    let x' = x + dist * intCos b.dir 
    let y' = y + dist * intSin b.dir 
    { b with pos = (x', y')}

let execute (c: Cmd) (b: WP ) : WP =
    match c with 
        | N dist -> moveNorth dist b
        | E dist -> moveEast dist b
        | W dist -> moveWest dist b
        | S dist -> moveSouth dist b
        | L deg -> rotateLeft b deg
        | R deg -> rotateRight b deg
        | F dist -> moveForward dist b 

//need a small refactor.

let rec executeCommands (cmds: Cmd list) (b: WP) : WP =
    match cmds with
        | c :: rest -> executeCommands rest (execute c b)
        | [] -> b    

let taxiCabFromOrigin (b:WP) : int = 
    let (x,y) = b.pos
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

let initialWp = { pos = (0,0); dir = 0}


[<Fact>]
let ``Boat moves correct with testdata`` () =
    let cmds = getCommands "data/test1.txt" |> Array.toList
    let b' = executeCommands cmds initialWp
    let (x',y') = b'.pos
    Assert.Equal(17, x')
    Assert.Equal(-8, y')
    Assert.Equal(25, taxiCabFromOrigin b')

[<Fact>]
let ``Boat moves correct with inputdata`` () =
    let cmds = getCommands "data/input.txt" |> Array.toList
    let b' = executeCommands cmds initialWp
    Assert.Equal(319, taxiCabFromOrigin b')
  
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
