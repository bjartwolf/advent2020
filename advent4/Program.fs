open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions

module PassportData =
    let parseInt (i:string) = match System.Int32.TryParse i with 
                                | true, v -> Some v
                                | false, _ -> None

    type BirthYear = private BirthYear of int 
    let tryCreateByr str : BirthYear option = 
        let year = parseInt str 
        match year with 
            | Some int when int <= 2002 && int >= 1920 -> Some (BirthYear int)
            | _ -> None 

    type IssueYear = private IssueYear of int 
    let tryCreateIyr str : IssueYear option = 
        let year = parseInt str 
        match year with 
            | Some int when int <= 2020 && int >= 2010 -> Some (IssueYear int)
            | _ -> None 

    type ExpirationYear = private ExpirationYear of int 
    let tryCreateEyr str : ExpirationYear option = 
        let year = parseInt str 
        match year with 
            | Some int when int <= 2030 && int >= 2020 -> Some (ExpirationYear int)
            | _ -> None 

    type Height = private Inches of int | Cm of int
    let tryCreateHgt (str: string) : Height option = 
        if (str.EndsWith("cm")) then
            let stripCm = str.Replace("cm", "") 
            let cm = parseInt stripCm 
            match cm with 
                | Some int when int <= 193 && int >= 150 -> Some (Cm int)
                | _ -> None 
        else if (str.EndsWith("in")) then
            let stripIn = str.Replace("in", "") 
            let inches = parseInt stripIn 
            match inches with 
                | Some int when int <= 76 && int >= 59 -> Some (Inches int)
                | _ -> None 
        else 
            None 
            
    type Haircolor = private Haircolor of string
    let tryCreateHcl (str: string) : Haircolor option = 
        let regex = Regex "^#[a-zA-Z0-9]{6}$"
        if (regex.IsMatch(str)) then
            Some (Haircolor str)
        else 
            None 

    type EyeColor = private EyeColor of string
    let tryCreateEcl (str: string) : EyeColor option = 
        let regex = Regex "^(amb|blu|brn|gry|grn|hzl|oth)$"
        if (regex.IsMatch(str)) then
            Some (EyeColor str)
        else 
            None 

    type PassportId = private PassportId of string
    let tryCreatePasportId (str: string) : PassportId option = 
        let regex = Regex "^[0-9]{9}$"
        if (regex.IsMatch(str)) then
            Some (PassportId str)
        else 
            None 

open PassportData
type PassPort = BirthYear * IssueYear * ExpirationYear * Height * Haircolor * EyeColor * PassportId

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        let sb = StringBuilder() 
        let readNextLine() =
            let line = sr.ReadLine ()
            sb.Append(" " + line) |> ignore
            String.IsNullOrWhiteSpace(line) |> not
        while readNextLine() do ignore |> ignore
        yield sb.ToString()
}

[<EntryPoint>]
let main argv =
    let lines = readLines "input4.txt" |> Seq.toList
    let splitKeyValByColon (l:string) = l.Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
                                            |> fun s -> (s.[0],s.[1])

    let createPassPortDictionary (l:string array) = 
        let foo = l |> Array.map splitKeyValByColon |> Array.toList
        dict foo
    
    let passPortHasKeys (p: IDictionary<string,string>) = 
        let keys = p.Keys
        keys.Contains("byr") && 
        keys.Contains("iyr") && 
        keys.Contains("eyr") && 
        keys.Contains("hgt") && 
        keys.Contains("hcl") && 
        keys.Contains("ecl") && 
        keys.Contains("pid")

    let createPassport (p: IDictionary<string,string>) =
        if passPortHasKeys p then 
            let birthYear = tryCreateByr p.["byr"]
            let issueYear = tryCreateIyr p.["iyr"]
            let expiratoinYear = tryCreateEyr p.["eyr"]
            let height = tryCreateHgt p.["hgt"]
            let haircolor = tryCreateHcl p.["hcl"]
            let eyecolor = tryCreateEcl p.["ecl"]
            let passportId = tryCreatePasportId p.["pid"]
            match birthYear, issueYear, expiratoinYear, height, haircolor, eyecolor, passportId with 
                | Some birthYear, Some issueYear, 
                  Some expiratoinYear, Some height,
                  Some haircolor, Some eyecolor,
                  Some passportId  -> Some (PassPort (birthYear, issueYear, expiratoinYear, height, haircolor, eyecolor, passportId))
                |  _ -> None
        else 
            None

    let createPassports (s: string) = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) 
                                      |> createPassPortDictionary

    lines |> List.map createPassports
          |> List.filter passPortHasKeys
          |> List.length
          |> printfn "%i"

    (List.map (createPassports >> (fun s -> s |> createPassport)) lines) 
        |> List.choose id
        |> List.length
        |> printfn "%A"
    0