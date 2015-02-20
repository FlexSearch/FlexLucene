#r "dll/FParsecCS.dll"
#r "dll/FParsec.dll"

open FParsec

// ------------------
// Data Structures
// ------------------
type JavaType = 
    { Namespace : string
      TypeName : string }

type FieldName = string

type MethodName = string

type FSharpName = string

type MethodParameters = JavaType list

type SpecialMethod = 
    | Constructor
    | Destructor

type Mapping = 
    | ClassMapping of JavaType * FSharpName
    | ConstructorDestructorMapping of SpecialMethod
    | FieldMapping of JavaType * FieldName * FSharpName
    | MethodMapping of JavaType * MethodName * MethodParameters * FSharpName

// ------------------
// Helper parsers
// ------------------
let ws = spaces
let str_ws s = pstringCI s .>> ws
let (<&&>) f1 f2 = (fun x -> f1 x && f2 x)
let (<||>) f1 f2 = (fun x -> f1 x || f2 x)

// -----------------------
// Data Structure Parsers
// -----------------------
let javaTypeParser = 
    let typeNameParser = manySatisfy (isLetter <||> (=) '$' <||> isDigit <||> (=) '[' <||> (=) ']' <||> (=) '_') .>> ws
    
    let nsParser = 
        let nsPart = manySatisfy (isLetter <||> isDigit)
        let nsPartDot = attempt <| pipe2 nsPart (pstring ".") (+)
        many nsPartDot |>> Seq.fold (+) ""
    pipe2 nsParser typeNameParser (fun ns t -> 
        { Namespace = ns
          TypeName = t })

let fSharpNameParser : Parser<FSharpName, _> = javaTypeParser |>> (fun jt -> jt.Namespace + jt.TypeName)
// Class Mapping
let classMappingParser = 
    pipe3 javaTypeParser (str_ws "->") fSharpNameParser (fun jt arrow name -> ClassMapping(jt, name)) .>> pstring ":"
let methodParamsParser : Parser<MethodParameters, _> = sepBy1 javaTypeParser (pstring ",")
let methodNameParser : Parser<MethodName, _> = manySatisfy (isLetter <||> isDigit <||> (=) '_' <||> (=) '$') .>> ws
// Method Mapping
let methodMappingParser = 
    ws 
    >>. pipe4 javaTypeParser methodNameParser (between (str_ws "(") (str_ws ")") methodParamsParser) 
            (str_ws "->" >>. fSharpNameParser) (fun jt mn mp fn -> MethodMapping(jt, mn, mp, fn))

// Special Method Mapping
let specialMethodMappingParser = 
    let construc = "<init>"
    let destruc = "<clinit>"
    let tor torType = 
        ws >>. javaTypeParser .>> pstring torType .>> between (str_ws "(") (str_ws ")") methodParamsParser 
        .>> str_ws "->" .>> str_ws torType
    attempt (construc
             |> tor
             |>> (fun _ -> ConstructorDestructorMapping(Constructor))) 
    <|> (destruc
         |> tor
         |>> (fun _ -> ConstructorDestructorMapping(Destructor)))

let fieldNameParser : Parser<FieldName, _> = methodNameParser
// Field Mapping
let fieldMappingParser = 
    ws 
    >>. pipe3 javaTypeParser fieldNameParser (str_ws "->" >>. fSharpNameParser) 
            (fun jt fn fsn -> FieldMapping(jt, fn, fsn))

// ------------------
// Main Parser
// ------------------
let Parse mappingFilePath = 
    let runParsers txt = 
        let parserOptions = 
            (attempt classMappingParser) <|> (attempt fieldMappingParser) <|> (attempt specialMethodMappingParser) 
            <|> (attempt methodMappingParser)
        match run parserOptions txt with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error
    System.IO.File.ReadAllLines(mappingFilePath) |> Seq.map runParsers

let getMethodsClassesAndFields mappings = 
    mappings |> Seq.filter (fun m -> 
                    match m with
                    | ClassMapping(_) -> true
                    | MethodMapping(_) -> true
                    | FieldMapping(_) -> true
                    | _ -> false)

// ---------------------
// Namespace transformer
// ---------------------
let nsParts ns = 
    let nsPart = manySatisfy (isLetter <||> isDigit)
    match run (sepBy1 nsPart (pstring ".")) ns with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error

let toTitleCase (s : string) = 
    match (s.ToCharArray()) |> Array.toList with
    | h :: t -> System.Char.ToUpper(h) :: t
    | _ -> []
    |> List.toArray
    |> (fun x -> System.String x)

let handleEnums (ns : string) typeName  =
    if ns.StartsWith("org.apache.lucene.util")
        || ns.StartsWith("org.apache.lucene.store")
    then typeName
    else
        let enumParser = 
            let classParser = manySatisfy (isLetter <||> isDigit <||> (=) '_')
            let enumParser = manySatisfy2 isLetter (isLetter <||> isDigit <||> (=) '_')
            pipe3
                classParser
                (pstring "$")
                enumParser
                (fun className _ enum -> className + "." + enum)
        match run (enumParser .>> eof) typeName with
        | Success (result,_,_) -> result
        | Failure (_,_,_) -> typeName

let toFlexNs (luceneNs : string) = 
    if luceneNs.StartsWith("org.apache.lucene.util") 
       || luceneNs.StartsWith("org.apache.lucene.store")
    then luceneNs
    else
        luceneNs.Replace("org.apache.lucene", "FlexLucene")
        |> nsParts
        |> Seq.filter ((<>) "")
        |> Seq.map toTitleCase
        |> Seq.fold (fun acc value -> acc + value + ".") ""

let toFlexMapping mapping = 
    match mapping with
    | ClassMapping(jt, fn) -> 
        let fn' = (jt.Namespace |> toFlexNs) + (jt.TypeName |> handleEnums jt.Namespace)
        ClassMapping(jt, fn')
    | MethodMapping(jt, mn, mp, fn) -> 
        if fn.StartsWith("replacementX")
        then MethodMapping(jt, mn, mp, mn |> toTitleCase)
        else MethodMapping(jt, mn, mp, fn)
    | FieldMapping(jt,fn,fsn) ->
        if fsn.StartsWith("replacementX") 
        then FieldMapping(jt,fn,fn |> toTitleCase)
        else FieldMapping(jt,fn,fsn)
    | _ -> failwith "This method mapping should not be here"

// ---------------------
// Actual Executor
// ---------------------
let replacementMappings mapping = 
    match mapping with
    | ClassMapping(_, fn) -> true
    | MethodMapping(_, _, _, fn) -> fn.StartsWith("replacementX")
    | FieldMapping(_,_,fsn) -> fsn.StartsWith("replacementX")
    | m' -> failwithf "This kind of mapping shouldn't be present %A" m'

let toProGuardMapping mapping = 
    let printJt jt = jt.Namespace + jt.TypeName
    let printMp mp = System.String.Join(",", mp |> Seq.map printJt)
    match mapping with
    | ClassMapping(jt, fn) -> sprintf "%s -> %s:" (printJt jt) fn
    | MethodMapping(jt, mn, mp, fn) -> sprintf "\t%s %s(%s) -> %s" (printJt jt) mn (printMp mp) fn
    | FieldMapping(jt, fn, fsn) -> sprintf "\t%s %s -> %s" (printJt jt) fn fsn
    | m -> failwithf "This kind of mapping shouldn't be present %A" m

let Execute mappingFilePath = 
    let newMappings = 
        Parse mappingFilePath
        |> getMethodsClassesAndFields
        |> Seq.filter replacementMappings
        |> Seq.map (toFlexMapping >> toProGuardMapping)
    System.IO.File.WriteAllLines(mappingFilePath + ".out", newMappings)
    newMappings |> Seq.length
