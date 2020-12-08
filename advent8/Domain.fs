module Domain
open Xunit

type Cmd = Nop | Jmp of int | Acc of int
type Program = Cmd list

type State = { PC : int;
               Acc: int;
               Visited: int list } 

let initState = { PC = 0;
                  Acc = 0;
                  Visited = []}

let evaluate (program: Program) (state: State) : State =
    let op = program.[state.PC]
    let res = match op with 
        | Acc a -> { state with Acc = state.Acc + a;
                                PC = state.PC + 1}
        | Jmp j -> { state with PC = state.PC + j }
        | Nop -> { state with PC = state.PC + 1 }
    
    { res with Visited = state.PC :: state.Visited }

[<Fact>]
let ``Evaluate 2 acc steps`` () =
    let program = [Acc 2; Acc 4] 
    let programEvaluator = evaluate program

    let output = evaluate program initState 
    Assert.Equal({ PC = 1;
                   Acc = 2;
                   Visited = [0] } , output )

    Assert.Equal({ PC = 2;
                   Acc = 6;
                   Visited = [1; 0] } , programEvaluator output )

[<Fact>]
let ``Evaluate 2 nop steps`` () =
    let program = [Nop; Nop] 
    let programEvaluator = evaluate program

    let output = evaluate program initState 
    Assert.Equal({ PC = 1;
                   Acc = 0;
                   Visited = [0] } , output )

    Assert.Equal({ PC = 2;
                   Acc = 0;
                   Visited = [1; 0] } , programEvaluator output )

[<Fact>]
let ``Evaluate 1 nop step`` () =
    let program = [Nop] 
    let programEvaluator = evaluate program

    let state = { PC = 0;
                  Acc = 0;
                  Visited = []}

    let output = programEvaluator state 
    Assert.Equal({ PC = 1;
                   Acc = 0;
                   Visited = [0] } , output )

