open System
open Xunit

type Operator = Plus | Mult | Nr of int
type Expressions = Operator list
type Stykke = int * Expressions 

let rec calc (es: Expressions) : int = 
    match es with 
        | Nr x :: Plus :: Nr y :: rest ->  calc ( Nr (x + y) :: rest)
        | Nr x :: Mult :: Nr y :: rest ->  calc ( Nr (x * y) :: rest ) 
        | [Nr x] -> x 

[<Fact>]
let ``foo`` () =
    Assert.Equal(3,calc [Nr 1 ; Plus; Nr 2])

    let foo : Expressions = [Nr 1 ; Plus; Nr 2; Mult; Nr 3 ; Plus ; Nr 4 ; Mult ; Nr 5 ; Plus ; Nr 6] 

    Assert.Equal(71,calc foo)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
