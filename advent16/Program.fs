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
type RuleWithDescription = string * Rule 
type RulesWithDescriptions = RuleWithDescription list 
type MatchingRule = RuleWithDescription * int list
type MatchingRules = MatchingRule list

let ruleFromUnparsedRule (up: UnparsedRule) : Rule =
    let (first_low, first_high,second_low,second_high) = up
    let range1 = { low = first_low; high = first_high }
    let range2 = { low = second_low; high = second_high }
    { r1 = range1;
      r2 = range2 } 

let ruleValidatorForSingleField (rs: RulesWithDescriptions) (field: int) : ErrorResult =
    let validRules = rs |> List.where (fun (_,r) -> ((field >= r.r1.low) && (field <= r.r1.high)) ||
                                                    ((field >= r.r2.low) && (field <= r.r2.high)))
    if (validRules|> Seq.isEmpty) then
        Some field 
    else
        None 
     
let validatorFromRule (rs: RulesWithDescriptions) : Ticket -> ErrorResult = 
    fun t -> 
        let brokenFields = seq {
            for field in t do
                    match ruleValidatorForSingleField rs field with
                        | None -> ()
                        | Some f -> yield f 
        }
        if (brokenFields |> Seq.isEmpty) then None
        else (brokenFields |> Seq.head |> Some) 

let rec errorsInTicket (t: Ticket) (rs: RulesWithDescriptions) : ErrorResult = 
    match t with 
        | t1 :: rest -> match (ruleValidatorForSingleField rs t1) with 
                            | None ->  errorsInTicket rest rs 
                            | Some error -> Some error 
        | [] -> None

let validateTicket (t: Ticket) (rs: RulesWithDescriptions) : Ticket option =
    match (errorsInTicket t rs) with 
        | Some _ -> None
        | None -> Some t

let validateTickets (ts: Tickets) (rs: RulesWithDescriptions) : Ticket list =
    ts 
        |> List.map (fun t -> validateTicket t rs)
        |> List.choose id

let sumFromTickets (ts: Tickets) (rs: RulesWithDescriptions) : int =
    ts 
        |> List.map (fun t -> errorsInTicket t rs) 
        |> List.choose id
        |> List.sum

let parseDescription (s:string) = 
    (s.Split(": ")).[0] 
     
let parseRule (s: string) = 
    let s' = (s.Split(": ")).[1] 
    let s'' = s'.Split(" or ")
    let r1 = s''.[0].Split("-")
    let r2 = s''.[1].Split("-")
    (int r1.[0], int r1.[1], int r2.[0], int r2.[1])

let parseTicket (s:string) =
    let rawTickets = s.Split(",") 
    rawTickets |> Array.map (int) |> Array.toList

let parseRuleWithDescription (s:string) =
    (parseDescription s, parseRule s) 

[<Fact>]
let ``parseDescription works`` () =
    Assert.Equal("departure location", parseDescription "departure location: 29-917 or 943-952") 
 
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
    
let parseRawRules (i: string array) : RulesWithDescriptions =
    i |> Array.map (parseRuleWithDescription) |> Array.toList 
      |> List.map (fun (d,r) -> (d,ruleFromUnparsedRule r))

let parsedRules : RulesWithDescriptions =
    parseRawRules (IO.File.ReadAllLines "rules.txt")

let parsedTickets: Tickets =
    let rawRules = IO.File.ReadAllLines "tickets.txt"
    rawRules |> Array.map (parseTicket) |> Array.toList 

[<Fact>]
let ``nr 1`` () = 
    Assert.Equal(23925, sumFromTickets parsedTickets parsedRules)


[<Fact>]
let ``filter out false rules`` () = 
    let remainingTickets = validateTickets parsedTickets parsedRules |> List.length
    Assert.Equal(190, remainingTickets)

let matchRuleWithTickets (ts: Tickets) (rs: RuleWithDescription) : MatchingRule =
    let rec transpose = function
        | [] -> failwith "cannot transpose a 0-by-n matrix"
        | [] :: xs -> [] 
        | xs -> List.map List.head xs :: transpose (List.map List.tail xs)
    let cols = transpose ts
    let validatedCols = cols |> List.mapi (fun i -> fun t -> (i, errorsInTicket t [rs]))
                             |> List.where (fun (i,e) -> e = None) |> List.map (fun (i,_) -> i)
    (rs, validatedCols) 
 
[<Fact>]
let ``example`` () = 
    let upr1 = (1,3,5,7)
    let upr2 = (6,11,33,44)
    let upr3 = (13,40,45,50)
    let rs: RulesWithDescriptions =
             [("foo", ruleFromUnparsedRule upr1);
              ("foo2", ruleFromUnparsedRule upr2);
              ("foo3", ruleFromUnparsedRule upr3) ]
    let nearbyTickets : Tickets = [ [7;3;47];
                                    [40;4;50];
                                    [55;2;20];
                                    [38;6;12] ]
    Assert.Equal(71, sumFromTickets nearbyTickets rs)

    let remainingTickets = validateTickets nearbyTickets rs 
    Assert.Equal(1, remainingTickets |> List.length)
    let matching = rs |> List.map (fun r -> matchRuleWithTickets nearbyTickets r)
    Assert.Equal(0, matching |> List.where (fun (r, l) -> l.Length > 0) |> List.length)

let rec findMatches (matches: MatchingRules) : MatchingRules =
    let singleMatch = matches |> List.where (fun (_,cols) -> cols.Length = 1) 
    if (singleMatch |> List.length) > 1 then failwith "Not sure i can do that"
    if (singleMatch |> List.length) = 0 then
         matches 
    else let (smr,m) = singleMatch |> List.head
         let matchedRow = m |> List.head
         let filteredMatches = 
            matches |> List.filter (fun (_ ,cols) -> cols.Length <> 1)  
                    |> List.map (fun ((d,r),cols) -> ((d,r), (cols |> List.filter (fun x -> x <> matchedRow))))
         (smr,m) :: (findMatches filteredMatches)
                                             
//    et removeThatFromTheOthers = matches |> List.map (fun ((d,r),cols) -> if (d = smr) then (//findMatchmatches 

[<Fact>]
let ``example 2`` () = 
    let rules = [| "class: 0-1 or 4-19"; "row: 0-5 or 8-19"; "seat: 0-13 or 16-19" |]
    
    let rs: RulesWithDescriptions = parseRawRules rules
    let nearbyTickets : Tickets = [ [3;9;18] ; [15;1;5]; [5;14;9]]

    let remainingTickets = validateTickets nearbyTickets rs 
    Assert.Equal(3, remainingTickets |> List.length)
    
    let matching = rs |> List.map (fun r -> matchRuleWithTickets nearbyTickets r)
    Assert.Equal(3, matching |> List.where (fun (r, l) -> l.Length > 0) |> List.length)
    let foo = findMatches matching
    Assert.Equal(2, foo |> List.length)



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
