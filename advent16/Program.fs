open System
open Xunit

type ErrorResult = int option
type UnparsedRule = int * int * int * int
type Range = {low: int; high: int} 
type Rule = {r1: Range; r2: Range} 
type Rules = Rule list
type Ticket = int list
type RuleValidatorForField = int -> ErrorResult 
type RuleValidator = Ticket -> ErrorResult 
type RuleValidators = RuleValidator list
type Tickets = Ticket list

let ruleFromUnparsedRule (up: UnparsedRule) : Rule =
    let (first_low, first_high,second_low,second_high) = up
    let range1 = { low = first_low; high = first_high }
    let range2 = { low = second_low; high = second_high }
    { r1 = range1;
      r2 = range2 } 

let ruleValidatorForSingleField (rs: Rules) (field: int) : ErrorResult =
    let validRules = rs |> List.where (fun r -> ((field >= r.r1.low) && (field <= r.r1.high)) ||
                                                ((field >= r.r2.low) && (field <= r.r2.high)))
    if (validRules|> Seq.isEmpty) then Some field 
    else None 
     
let validatorFromRule (rs: Rules) : Ticket -> ErrorResult = 
    fun t -> 
        let brokenFields = seq {
            for field in t do
                    match ruleValidatorForSingleField rs field with
                        | None -> ()
                        | Some f -> yield f 
        }
        if (brokenFields |> Seq.isEmpty) then None
        else (brokenFields |> Seq.head |> Some) 

let rec errorsInTicket (t: Ticket) (rs: Rules) : ErrorResult = 
    match t with 
        | t1 :: rest -> match (ruleValidatorForSingleField rs t1) with 
                            | None ->  errorsInTicket rest rs 
                            | Some error -> Some error 
        | [] -> None

[<Fact>]
let ``example`` () = 
    let upr1 = (1,3,5,7)
    let upr2 = (6,11,33,44)
    let upr3 = (13,40,45,50)
    let rs = [ruleFromUnparsedRule upr1;
              ruleFromUnparsedRule upr2;
              ruleFromUnparsedRule upr3 ]
    // let ticket : Ticket = [7;1;4] ignore own ticket for now
    let nearbyTickets : Tickets = [ [7;3;47];
                                    [40;4;50];
                                    [55;2;20];
                                    [38;6;12] ]
    let errorResults = nearbyTickets 
                        |> List.map (fun t -> errorsInTicket t rs) 
                        |> List.choose id
    let sum = errorResults |> List.sum
    Assert.Equal(71, sum)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
