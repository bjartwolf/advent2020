open System.IO

let numbers = File.ReadLines("input.txt") |> Seq.map int |> Seq.toList

let crossproduct l1 l2 l3 =
  seq { for el1 in l1 do
          for el2 in l2 do
            for el3 in l3 do
                yield el1, el2, el3 }

let numberPairs = crossproduct numbers numbers numbers

let twentytwenty = numberPairs 
                    |> Seq.filter (fun (x,y,z) -> x + y + z = 2020) 
                    |> Seq.take 1
                    |> Seq.map (fun (x,y,z) -> (x,y,z,x+y+z,x*y*z))

printf "%A" twentytwenty