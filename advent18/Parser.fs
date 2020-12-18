module Parser
open Domain

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
        if (indexOfMult = -1 && indexOfPlus = -1) then [Nr (int64 s)] 
        else if (indexOfPlus <> -1 && (indexOfMult = -1 || indexOfPlus < indexOfMult )) then 
            let s' = s.Substring(0,indexOfPlus)
            let s'' = s.Substring(indexOfPlus + 1)
            Nr (int64 s') ::  Plus :: parse s''
        else
            let s' = s.Substring(0,indexOfMult)
            let s'' = s.Substring(indexOfMult + 1)
            Nr (int64 s') ::  Mult :: parse s''
