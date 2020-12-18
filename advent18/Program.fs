open System
open Xunit

type Exp = Plus | Mult | Nr of int | Exp of Exp list 
type Expressions = Exp list

let rec calc (es: Expressions) : int = 
    match es with 
        | a :: b :: Exp x :: rest ->  calc ( a :: b :: Nr (calc x) :: rest)
        | Exp x :: a :: b :: rest ->  calc ( Nr (calc x) :: a :: b :: rest)
        | Nr x :: Plus :: Nr y :: rest ->  calc ( Nr (x + y) :: rest)
        | Nr x :: Mult :: Nr y :: rest ->  calc ( Nr (x * y) :: rest ) 
        | [Nr x] -> x 
        | [Exp x] -> calc x 

[<Fact>]
let ``foo`` () =
    let exp = [Nr 1 ; Plus; Nr 2]
    Assert.Equal(3,calc exp)

    Assert.Equal(5, calc [Exp [Nr 2; Plus; Nr 3]]) 

    let exp = [Nr 1 ; Plus; Exp [Nr 2; Plus; Nr 3]]
    Assert.Equal(6,calc exp) 

    let foo : Expressions = [Nr 1 ; Plus; Nr 2; Mult; Nr 3 ; Plus ; Nr 4 ; Mult ; Nr 5 ; Plus ; Nr 6] 
    Assert.Equal(71,calc foo)

    Assert.Equal(51, calc [Nr 1; Plus ; Exp [Nr 2 ; Mult; Nr 3]; Plus ; Exp [Nr 4 ; Mult ; Exp [ Nr 5; Plus; Nr 6]]])

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
