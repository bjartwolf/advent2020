module Domain
open Xunit

type Cmd = Nop | Jmp of int | Acc of int
type Program = Cmd list

type State = { PC : int;
               Acc: int;
               Visited: int list } 

let isCompleted (program: Program) (state: State) : bool =
    let isFinished = state.PC + 1 >= program.Length 
    let hasRunNextCommandBefore = List.contains (state.PC + 1) state.Visited
    isFinished || hasRunNextCommandBefore

let initState = { PC = -1;
                  Acc = 0;
                  Visited = [] }

let evaluate (program: Program) (state: State) : State =
    let pc = state.PC + 1
    let op = program.[pc]
    let res = 
        match op with 
            | Acc a -> { state with 
                                Acc = state.Acc + a;
                                PC = pc}
            | Jmp j -> { state with 
                                PC = state.PC + j } // it seems like jmp 1 equal nop
            | Nop   -> { state with PC = pc }
        
    { res with Visited = pc :: state.Visited }

let evaluateProgram (program: Program) : State =
    let mutable state = initState // yeah, I know
    let programEvaluator = evaluate program
    while (not (isCompleted program state)) do 
        state <- programEvaluator state
    state 

[<Fact>]
let ``Evaluate 1 nop step automatically`` () =
    let program = [Nop] 
    let result = evaluateProgram program
    Assert.Equal({ PC = 0;
                   Acc = 0;
                   Visited = [0] } , result)

    Assert.True(isCompleted program result)


[<Fact>]
let ``Evaluate 1 acc and 1 jump steps`` () =
    let program = [Acc 2; Jmp 2; Acc 4; Acc 13] 
    let programEvaluator = evaluate program
    let output = evaluate program initState 

    Assert.Equal({ PC = 0;
                   Acc = 2;
                   Visited = [0] } , output )

    let final = output |> programEvaluator |> programEvaluator

    Assert.Equal({ PC = 3;
                   Acc = 15;
                   Visited = [3; 1; 0] } , final )
    Assert.True(isCompleted program final)

[<Fact>]
let ``Evaluate 2 acc steps`` () =
    let program = [Acc 2; Acc 4] 
    let programEvaluator = evaluate program

    let output = evaluate program initState 
    Assert.Equal({ PC = 0;
                   Acc = 2;
                   Visited = [0] } , output )

    Assert.False(isCompleted program output)

    let nextState = programEvaluator output
    Assert.Equal({ PC = 1;
                   Acc = 6;
                   Visited = [1; 0] } , nextState)
    Assert.True(isCompleted program nextState)

[<Fact>]
let ``Evaluate 2 nop steps`` () =
    let program = [Nop; Nop] 
    let programEvaluator = evaluate program

    let output = evaluate program initState 
    Assert.Equal({ PC = 0;
                   Acc = 0;
                   Visited = [0] } , output )

    Assert.False(isCompleted program output)

    let nextState = programEvaluator output 
    Assert.Equal({ PC = 1;
                   Acc = 0;
                   Visited = [1; 0] }, nextState)

    Assert.True(isCompleted program nextState)

[<Fact>]
let ``Evaluate 1 nop step`` () =
    let program = [Nop] 
    let programEvaluator = evaluate program

    let output = programEvaluator initState 
    Assert.Equal({ PC = 0;
                   Acc = 0;
                   Visited = [0] } , output )

    Assert.True(isCompleted program output)
