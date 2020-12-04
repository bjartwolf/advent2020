open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions

module PassportData =
    let private parseInt (i:string) = 
        match System.Int32.TryParse i with 
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
    
    let inchesRegex = Regex "^([0-9]{2})in$"
    let cmRegex = Regex "^([0-9]{3})cm$"

    let (|IN |_|) input =
       let m = inchesRegex.Match(input) 
       if (m.Success) then Some (int m.Groups.[1].Value) else None  

    let (|CM|_|) input =
       let m = cmRegex.Match(input) 
       if (m.Success) then Some (int m.Groups.[1].Value) else None  
    
    let between min max value = value >= min && value <= max 

    type Height = private Inches of int | Cm of int
    let tryCreateHgt (str: string) : Height option = 
        match str with 
            | CM cm -> if cm |> between 150 193 then Some (Cm cm)
                              else None
            | IN inches -> if inches |> between 59 76 then Some (Inches inches)
                           else None
            | _ -> None
    let private hairRegex = Regex "^#[a-zA-Z0-9]{6}$"
    type Haircolor = private Haircolor of string
    let tryCreateHcl (str: string) : Haircolor option = 
        if (hairRegex.IsMatch(str)) then
            Some (Haircolor str)
        else 
            None 

    type EyeColor = private EyeColor of string
    let private eyeRegex = Regex "^(amb|blu|brn|gry|grn|hzl|oth)$"
    let tryCreateEcl (str: string) : EyeColor option = 
        if (eyeRegex.IsMatch(str)) then
            Some (EyeColor str)
        else 
            None 

    type PassportId = private PassportId of string
    let private passportRegex = Regex "^[0-9]{9}$"
    let tryCreatePasportId (str: string) : PassportId option = 
        if (passportRegex.IsMatch(str)) then
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
        keys.Contains("byr") && keys.Contains("iyr") && 
        keys.Contains("eyr") && keys.Contains("hgt") && 
        keys.Contains("hcl") && keys.Contains("ecl") && 
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

    let createPassportDictionaries (s: string) = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) 
                                                  |> createPassPortDictionary

    let passportsWithAllKeys = lines |> List.map createPassportDictionaries
                                     |> List.filter passPortHasKeys

    passportsWithAllKeys |> List.length
                         |> printfn "%i"

    passportsWithAllKeys
        |> List.map createPassport
        |> List.choose id
        |> List.length
        |> printfn "%A"
    0