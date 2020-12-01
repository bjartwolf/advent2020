#r "nuget:FSharp.Collections.ParallelSeq"

open System.IO
open FSharp.Collections.ParallelSeq


// File.ReadLines relies on enumerations finally block to dispose stream 
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let crossproduct l1 l2 l3 =
  seq { for el1 in l1 do
          for el2 in l2 do
            for el3 in l3 do
                yield int el1, int el2, int el3 }

let numbers = readLines "input.txt" 
let twentytwenty = crossproduct numbers numbers numbers
                    |> PSeq.filter (fun (x,y,z) -> x + y + z = 2020) 
                    |> Seq.take 1
                    |> PSeq.map (fun (x,y,z) -> (x,y,z,x+y+z,x*y*z))

printf "%A" twentytwenty