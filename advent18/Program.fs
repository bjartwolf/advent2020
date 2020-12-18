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

let findClosingParen (s:string) : int = 
    let mutable count = 1
    let mutable i = 0
    while (count > 0) do
        if s.[i] = '(' then
           count <- count + 1 
        else if s.[i] = ')' then
            count <- count - 1
        else
            ()
        i <- i + 1
    i - 1

[<Fact>]
let ``find close works`` () =
    Assert.Equal(2, findClosingParen "())")
    Assert.Equal(2, findClosingParen "())()")
    Assert.Equal(4, findClosingParen "(()))()")
    Assert.Equal(8, findClosingParen "((()))())(())")
    Assert.Equal(0, findClosingParen ")")
 
let rec parse (s1: string) : Expressions =
    let s = s1.TrimStart()
    if s = "" then []
    else if s.[0] = '*' then Mult :: parse (s.Substring(1))
    else if s.[0] = '+' then Plus :: parse (s.Substring(1))
    else if (s.[0] = '(') then 
        let closeParen = findClosingParen(s.Substring(1)) 
        Exp (parse (s.Substring(1,closeParen))) :: parse (s.Substring(closeParen + 2))
    else 
        let indexOfPlus = s.IndexOf('+')
        let indexOfMult = s.IndexOf('*')
        if (indexOfMult = -1 && indexOfPlus = -1) then [Nr (int s)] 
        else if (indexOfPlus <> -1 && (indexOfMult = -1 || indexOfPlus < indexOfMult )) then 
            let s' = s.Substring(0,indexOfPlus)
            let s'' = s.Substring(indexOfPlus + 1)
            Nr (int s') ::  Plus :: parse s''
        else
            let s' = s.Substring(0,indexOfMult)
            let s'' = s.Substring(indexOfMult + 1)
            Nr (int s') ::  Mult :: parse s''

[<Fact>]
let ``Parser works`` () =
   let answer2 = ([Exp [Nr 1; Plus; Exp [ Nr 3; Mult; Nr 5]]] = (parse "(1 + (3 * 5))"))
   Assert.True(answer2)
   let answer1 = ([Exp [Nr 1; Plus; Nr 3]] = (parse "(1 + 3)"))
   Assert.True(answer1)
   let answer3 = ([Nr 1; Plus; Nr 3; Plus; Nr 5] = (parse "1 + 3 + 5"))
   Assert.True(answer3)
   let answer4 = ([Nr 1; Plus; Nr 3; Plus; Nr 5; Mult; Nr 6] = (parse "1 + 3 + 5 * 6"))
   Assert.True(answer4)

[<Fact>]
let ``Calc on strings works`` () =
    Assert.Equal(13632, calc (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
    Assert.Equal(71, calc (parse "1 + 2 * 3 + 4 * 5 + 6"))

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
