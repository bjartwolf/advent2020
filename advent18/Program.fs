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

[<Fact>]
let ``Calcuations works`` () =
    Assert.Equal(71,calc [Nr 1 ; Plus; Nr 2; Mult; Nr 3 ; Plus ; Nr 4 ; Mult ; Nr 5 ; Plus ; Nr 6] )
    Assert.Equal(51, calc [Nr 1; Plus ; Exp [Nr 2 ; Mult; Nr 3]; Plus ; Exp [Nr 4 ; Mult ; Exp [ Nr 5; Plus; Nr 6]]])
    Assert.Equal(26, calc [Nr 2; Mult; Nr 3; Plus; Exp [Nr 4; Mult; Nr 5]]) 
    Assert.Equal(437, calc [Nr 5; Plus ; Exp [Nr 8 ; Mult ; Nr 3; Plus; Nr 9; Plus ; Nr 3; Mult; Nr 4; Mult; Nr 3]])
    Assert.Equal(12240, calc [Nr 5; Mult; Nr 9; Mult; Exp[Nr 7 ; Mult; Nr 3; Mult; Nr 3; Plus; Nr 9; Mult; Nr 3; Plus; Exp [Nr 8; Plus; Nr 6; Mult; Nr 4 ]]])
    Assert.Equal(13632, calc [Exp[Exp[ Nr 2; Plus; Nr 4; Mult; Nr 9]; Mult; Exp [Nr 6; Plus; Nr 9; Mult; Nr 8; Plus; Nr 6]; Plus; Nr 6]; Plus; Nr 2; Plus; Nr 4; Mult; Nr 2])

let rec parse (s: string) : Expressions =
    let indexOfPlus = s.IndexOf('+')
    let indexOfMult = s.IndexOf('*')
    if (indexOfMult = -1 && indexOfPlus = -1) then [Nr (int s)] 
    else let s' = s.Split("+") 
         printfn "%A" s'
         [Nr (int s'.[0]) ; Plus ; Nr (int s'.[1])]

[<Fact>]
let ``Parser works`` () =
   let answer2 = ([Nr 1; Plus; Nr 3] = (parse "1 + 3"))
   Assert.True(answer2)
   let answer = [Nr 1] = (parse "1")
   Assert.True(answer)
 
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
