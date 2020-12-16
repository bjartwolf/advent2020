open System
open Xunit

type ErrorResult = int option
type UnparsedRule = int * int * int * int
type Range = {low: int; high: int} 
type Rule = {r1: Range; r2: Range} 
type Rules = Rule list
type Ticket = int list
type RuleValidator = Ticket -> ErrorResult 
type RuleValidators = RuleValidator list
type Tickets = Ticket list

let ruleFromUnparsedRule (up: UnparsedRule) : Rule =
    let (first_low, first_high,second_low,second_high) = up
    let range1 = { low = first_low; high = first_high }
    let range2 = { low = second_low; high = second_high }
    { r1 = range1;
      r2 = range2 } 

let validatorFromRule (r: Rule) : RuleValidator =
    fun t -> 
        let brokenFields = seq {
            for field in t do
                if (field < r.r1.low) then yield field
                else if (field > r.r1.high) then yield field
                else if (field < r.r2.low) then yield field
                else if (field > r.r2.high) then yield field
        }
        if (brokenFields |> Seq.isEmpty) then None
        else (brokenFields |> Seq.head |> Some) 

let validatorFromUnparsedRule = ruleFromUnparsedRule >> validatorFromRule

(* let isTicketValid (t: Ticket) (rvs: RuleValidators) : bool =
    let res = rvs |> List.map (fun rv -> rv t) 
    let errors = res |> List.choose id
    if (List.isEmpty errors) then true else false *)

let rec errorsInTicket (t: Ticket) (rvs: RuleValidators) : ErrorResult = 
    match rvs with 
        | rv :: rest -> if (rv t) = None then 
                            errorsInTicket t rest 
                        else
                            rv t
        | [] -> None

[<Fact>]
let ``example`` () = 
    let unparsedRule1 = (1,3,5,7)
    let v1 = validatorFromUnparsedRule unparsedRule1
    let unparsedRule2 = (6,11,33,44)
    let v2 = validatorFromUnparsedRule unparsedRule2
    let unparsedRule3 = (13,40,45,50)
    let v3 = validatorFromUnparsedRule unparsedRule3
    let validators = [v1;v2;v3]
    // let ticket : Ticket = [7;1;4] ignore own ticket for now
    let nearbyTickets : Tickets = [ [7;3;47];
                                    [40;4;50];
                                    [55;2;20];
                                    [38;6;12] ]
    let errorResults = nearbyTickets 
                        |> List.map (fun t -> errorsInTicket t validators) 
                        |> List.choose id
    let sum = errorResults |> List.sum
    Assert.Equal(71, sum)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
