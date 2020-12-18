open System
open Xunit
open Parser
open Domain

let rec calc (es: Expressions) : int64 = 
    match es with 
        | a :: b :: Exp x :: rest ->  calc ( a :: b :: Nr (calc x) :: rest)
        | Exp x :: a :: b :: rest ->  calc ( Nr (calc x) :: a :: b :: rest)
        | Nr x :: Plus :: Nr y :: rest ->  calc ( Nr (x + y) :: rest)
        | Nr x :: Mult :: Nr y :: rest ->  calc ( Nr (x * y) :: rest ) 
        | [Nr x] -> x 
        | [Exp x] -> calc x 


let rec addParens (exps: Expressions) : Expressions =
    match exps with 
        | Nr a :: Plus :: Nr b :: rest -> Exp [Nr a ; Plus ; Nr b] :: addParens rest
        | Nr a :: Plus :: rest -> [Exp [Nr a ; Plus ; Exp (addParens rest)]]
        | Exp a :: Plus :: rest -> [Exp [Exp (addParens a) ; Plus ; Exp (addParens rest)]]
        | Nr a :: Mult:: rest -> Nr a :: Mult :: addParens rest 
        | Mult:: rest -> Mult :: addParens rest 
        | Plus :: rest -> Plus :: addParens rest 
        | Exp a :: rest -> Exp (addParens a) :: addParens rest 
        | [a] -> [a] 
        | [] -> [] 
 
[<Fact>]
let ``Add parens works`` () =
   let s0 = "9 + 4" 
   let exp0 = addParens (parse s0)
   let eq0 = exp0 = [Exp [Nr 9L; Plus; Nr 4L]]
   Assert.True(eq0)

   let s1 = "2 * 9 + 4" 
   let exps = addParens (parse s1)
   let eq = exps = [Nr 2L; Mult; Exp [Nr 9L; Plus; Nr 4L]]
   Assert.True(eq)

   let s2 = "9 + 4 * 3 + 9" 
   let exp2 = addParens (parse s2)
   let eq2= exp2 = [Exp [Nr 9L; Plus; Nr 4L]; Mult; Exp [Nr 3L; Plus; Nr 9L]]
   Assert.True(eq2)

[<Fact>]
let ``Calc on strings works`` () =
    Assert.Equal(13632L, calc (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
    Assert.Equal(71L, calc (parse "1 + 2 * 3 + 4 * 5 + 6"))

[<Fact>]
let ``Calc on strings works 2`` () =
    Assert.Equal(46L, calc (addParens (parse "2 * 3 + (4 * 5)")))
    Assert.Equal(156L, calc (addParens (parse "9 + 4 * 3 + 9"))) 
  //  Assert.Equal(1445L, calc  (addParens (parse "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  //  Assert.Equal(51L, calc (addParens (parse "1 + (2 * 3) + (4 * (5 + 6))")))
  //  Assert.Equal(23340L, calc (addParens (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))

[<Fact>]
let ``Nr 1 works`` () =
    let file = IO.File.ReadAllLines "data/input.txt"
    let res = file |> Array.map (fun l ->  calc (parse l)) |> Array.map (int64) |> Array.sum
    Assert.Equal(280014646144L, res)

[<EntryPoint>]
let main argv =
    let file = IO.File.ReadAllLines "data/input.txt"
    let res = file |> Array.map (fun l ->  calc (parse l)) |> Array.map (int64) |> Array.sum
    printfn "Sum is %A" res
    0 // return an integer exit code
