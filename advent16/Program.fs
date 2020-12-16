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
    if (validRules|> Seq.isEmpty) then
        Some field 
    else
        None 
     
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

let sumFromTickets (ts: Tickets) (rs: Rules) : int =
    ts 
        |> List.map (fun t -> errorsInTicket t rs) 
        |> List.choose id
        |> List.sum


let parseRule (s: string) = 
    let s' = (s.Split(": ")).[1] 
    let s'' = s'.Split(" or ")
    let r1 = s''.[0].Split("-")
    let r2 = s''.[1].Split("-")
    (int r1.[0], int r1.[1], int r2.[0], int r2.[1])

let parseTicket (s:string) =
    let rawTickets = s.Split(",") 
    rawTickets |> Array.map (int) |> Array.toList

[<Fact>]
let ``parseTicketWorks`` () =
    let t = [917;157;627;684;64;737;544;626;363;77;742;911;781;358;138;253;545;93;95;500]
    let rawTicket = "917,157,627,684,64,737,544,626,363,77,742,911,781,358,138,253,545,93,95,500"
    let ok = t = parseTicket rawTicket
    Assert.True(ok)
   
[<Fact>]
let ``parseRulesWorks`` () =
    let r1 = parseRule "departure location: 29-917 or 943-952"
    let match1 = r1 = (29,917,943,952)
    Assert.True(match1)
    let r2 = parseRule "departure station: 50-875 or 884-954"
    let match2 = r2 = (50,875,884,954)
    Assert.True(match2)
    
let parsedRules : Rules =
    let rawRules = IO.File.ReadAllLines "rules.txt"
    rawRules |> Array.map (parseRule) |> Array.toList 
             |> List.map (ruleFromUnparsedRule)

let parsedTickets: Tickets =
    let rawRules = IO.File.ReadAllLines "tickets.txt"
    rawRules |> Array.map (parseTicket) |> Array.toList 

[<Fact>]
let ``nr 1`` () = 
    Assert.Equal(23925, sumFromTickets parsedTickets parsedRules)

[<Fact>]
let ``example`` () = 
    let upr1 = (1,3,5,7)
    let upr2 = (6,11,33,44)
    let upr3 = (13,40,45,50)
    let rs = [ruleFromUnparsedRule upr1;
              ruleFromUnparsedRule upr2;
              ruleFromUnparsedRule upr3 ]
    let nearbyTickets : Tickets = [ [7;3;47];
                                    [40;4;50];
                                    [55;2;20];
                                    [38;6;12] ]
    Assert.Equal(71, sumFromTickets nearbyTickets rs)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
