module Routing
open System
open System.Text.RegularExpressions
//open Json

type userInput = Valid of string | Invalid of string;;
type middlewareResult = Siga of Map<string,string> | NoSiga of string;;   
let validationAccepting transformFn = 
   (fun preed ->    
     match preed with
     | Invalid(s) -> Invalid(s)
     | Valid(strr) -> transformFn(strr));;   
let noSigaAccepting middlewareFn =                                                
   (fun preed ->
     match preed with
     | NoSiga(x) -> NoSiga(x)
     | Siga(y) -> (middlewareFn y));;   
let htmlEntities =
    Map.empty.
        Add("&", "&amp;").
        Add(".", "&#46;").
        Add("+", "&#43;").
        Add(";", "&#59;");;   
let dynamoEntities =
    Map.empty.
       Add("EQ", "&#69;&#81;").
       Add("NE", "&#78;&#69;").
       Add("IN", "&#73;&#78;").
       Add("LE", "&#76;&#69;").
       Add("LT", "&#76;&#84;").
       Add("GE", "&#71;&#69;").
       Add("GT", "&#71;&#84;").
       Add("BETWEEN", "&#66;&#69;&#84;&#87;&#69;&#69;&#78;").
       Add("NULL", "&#78;&#85;&#76;&#76;").
       Add("CONTAINS", "&#67;&#79;&#78;&#84;&#65;&#73;&#78;&#83;");;        
let keyz mapa = mapa |> Map.toSeq |> Seq.map fst;;

let subs rx (entityMap: Map<string, string>) str = // substitutions based on pattern and lookup map (for actual values to substitute)
     Regex.Replace(str, rx,
         (fun (x : Match) -> entityMap.[x.Value] ),
         RegexOptions.IgnoreCase) ;;  // wait, don't do this
type reqMapNode =
    | StrVal of string 
    | NumVal of int
    | NumFVal of float
    | MapRef of reqMap
    | AssocListRef of (string * reqMapNode) list // can be transformed to/from MapRef
    | ListRef of reqMapNode list
and reqMap = Map<string, reqMapNode>;;
let basicRequest =
    Map.empty.
       Add("k1", Num(2)).
       Add("k2", ListRef([Num(2); Str("yo, dude")]));;
let basicRequestParent =
    Map.empty.
       Add("k4", Num(3)).
       Add("k3", MapRef(basicRequest));;  
let rec recompose (acc: Map<string,string>) tupleSeq =  // list of tuples to immutable map
     match (Seq.toList tupleSeq) with
     | [] -> acc
     | (k,v) :: t -> (recompose (acc.Add(k,v)) t) ;;
let dynamoEntityRegex = "(EQ|NE|IN|LE|LT|GE|GT|BETWEEN|NULL|CONTAINS)"
let htmlEntityRegex = "(\.|\+|&|;)- "
let dynamoPatternValidator = (subs "(EQ|NE|IN|LE|LT|GE|GT|BETWEEN|NULL|CONTAINS)" dynamoEntities) // curried function call, returns a function still
let htmlEntityValidator = (subs "(\.|\+|&|;)- "  htmlEntities)
let transformEntities (req: Map<string,string>) = 
    let myKeyz = (keyz req)   
    myKeyz |> Seq.map (fun x -> (x, req.[x] |> dynamoPatternValidator |> htmlEntityValidator)) |> List.ofSeq;;     
let transformEntitiesMap (req: reqMap) =
    let myKeyz = (keyz req)   
    myKeyz |> Seq.map (fun x ->
        match req.[x] with
        | MapRef(mapa) -> (x, AssocListRef((transformEntitiesMap mapa)))
        | ListRef(lista) -> (x,  ListRef((transformListOfEntities lista)))
        | StrVal(s) -> (x, s |> dynamoPatternValidator |> htmlEntityValidator |> (fun s2 -> StrVal(s2)) )
        | NumVal(n) -> (x, NumVal(n))
        | NumFVal(n) -> (x, NumFVal(n))
        ) |> List.ofSeq
and transformListOfEntities (entityList: reqMapNode list) = // returns a list, not tagged...
    (List.map (fun listNode -> 
        match node with
        | MapRef(mapa) -> AssocListRef((transformEntitiesMap mapa)) // tag???
        | ListRef(lista2) -> ListRef((transformListOfEntities lista2))  // tag???
        | StrVal(s) -> s |> dynamoPatternValidator |> htmlEntityValidator |> (fun st -> StrVal(st))
        | NumVal(n) -> NumVal(n)
        | NumFVal(n) -> NumFVal(n)
        ) entityList)
let rec assocListToMap (acc: Map<string,string>) tupleSeq =  // list of tuples to immutable map
     match (Seq.toList tupleSeq) with
     | [] -> acc
     | (k,NumFVal(n)) :: t -> (assocListToMap (acc.Add(k,NumFVal(n))) t)
     | (k,NumVal(n)) :: t -> (assocListToMap (acc.Add(k,NumVal(n))) t)
     | (k,StrVal(s)) :: t -> (assocListToMap (acc.Add(k,StrVal(s))) t)
     | (k,ListRef(s)) :: t -> (assocListToMap (acc.Add(k,ListRef(s))) t)
     | (k,AssocListRef(mapAsList)) :: t -> (assocListToMap (acc.add(k,MapRef((assocListToMap Map.empty mapAsList)))) t);;
let transformFull (r: Map<string, string>) =
    (transformEntities r) |> (recompose Map.empty) |> (fun s -> Siga(s));;

let lengthValidate s =
    match ((String.length s) < 50) with
    | true -> Valid(s)
    | false -> Invalid("too long");;
let unsafeEntities = ["NOT_NULL"; "NOT_CONTAINS"; "BEGINS_WITH"];; 
let strContain (x:string, y: string) = y.Contains(x)
let preedString st = 
     match  (List.exists (fun en -> strContain(st,en)) unsafeEntities) with
     | true -> Invalid("unsafe!")
     | false -> Valid(st);;
let validateEntities (req: Map<string,string>) = 
    let myKeyz = (keyz req)   
    myKeyz |> Seq.map (fun x -> (x, Valid(req.[x]) |> (validationAccepting preedString)  |> (validationAccepting lengthValidate))) |> List.ofSeq;;
type validatedMap = Valido of (string * string) list | Invalido of string;;
let reqq =
    Map.empty.
        Add("a", "yo").
        Add("b", "sdfdsf,,...").
        Add("c", "dafs").
        Add("d", "dfdd");;  

let rec validateMap acc mylst =
    match mylst with
    | [] -> Valido(acc)
    | (_, Invalid(ss)) :: _ -> Invalido(ss)
    | (k, Valid(s)) :: t -> (validateMap  ((k,s)::acc)  t) ;;    
let validateFull (req: Map<string, string>) = req |> validateEntities |> (validateMap []);;    

let validatePipeline (req: Map<string, string>) =
   match (validateFull req) with
   | Invalido(ss) -> NoSiga("nahhhh")
   | Valido(ss) -> (
                     let u = ss |> (recompose Map.empty)
                     Siga(u) );;
let asyncMw mwFun next errorFun = 
  (fun req ->
    let callResult = ((noSigaAccepting h) req)
    match callResult with
    | NoSiga(msg) -> (errFun NoSiga(msg))
    | Siga(reqNew) -> (next req) // has no own next/errorfn
  )          
let chainMw mwList errorFun =
   match mwList with
   | [] -> (fun(req) -> Siga(req)) // necessary?
   | h :: t -> (asyncMw h (chainMw t errorFun) errorFun);;  
let rec applyMw taggedReq mwList =
   match mwList with
   | [] -> taggedReq
   | h :: t -> (applyMw ((noSigaAccepting h) taggedReq) t);;
let _ = (applyMw (Siga(reqq)) [transformFull; validatePipeline]);;   




//XTRA
    let checkJwtClaims3 (req: APIGatewayProxyRequest) (claimsToCheck: string list) =
        let lookupValue = req.Headers.ContainsKey("Authorization")
        match lookupValue with 
        | false -> NoSiga("no Authorization Header")
        | true -> (
                    let authHeader = req.Headers.["Authorization"]
                    match authHeader with
                    | Regex @"(Bearer )(.*)" [_; v] -> (
                                                                
                                                            //let decodedClaimsList = decode<UserRights>(v) |> (fun x -> x.Claims |> List.ofArray);
                                                            let decodedClaimsList = decode<UserRights>(v) |> (fun x -> x.Claims |> List.ofArray);
                                                            let existingClaims = (List.map (fun claimToCheck ->       (List.exists (fun decodedClaim -> decodedClaim = claimToCheck)  decodedClaimsList)        ) claimsToCheck)
                                                            let anyClaimsAbsent = List.exists (fun c -> c = false) existingClaims;
                                                            match anyClaimsAbsent with
                                                            | true -> NoSiga("JWT missing come claims")
                                                            | false -> Siga(req)       
                                                            ))                                                         
                    | _ -> NoSiga("malformed auth header" );;
        
    let checkJwtClaims2 (req: Map<string, string>) (claimsToCheck: string list) =
        let jwtValOpt = getValFromKey req "jwtToken"
        match jwtValOpt with
        | None -> NoSiga("JWT lookup fail - not in req. it needs the key 'jwtToken'")
        | Some(v) -> (
            let decodedClaimsList = decode<UserRights>(v) |> (fun x -> x.Claims |> List.ofArray);
            let existingClaims = List.map (fun claimToCheck -> (List.exists (fun decodedClaim -> decodedClaim = claimToCheck)  decodedClaimsList) claimsToCheck;
            let anyClaimsAbsent = List.exists (fun c -> c = false) existingClaims;
            match anyClaimsAbsent with
            | true -> NoSiga("JWT missing come claims")
            | false -> Siga(req)
            );;        


// MO
    let checkJwtClaims2 (req: Map<string, string>) (claimsToCheck: string list) =
        let jwtValOpt = getValFromKey req "jwtToken"
        match jwtValOpt with
        | None -> NoSiga("JWT lookup fail - not in req. it needs the key 'jwtToken'")
        | Some(v) -> (
            let decodedClaimsList = decode<UserRights>(v) |> (fun x -> x.Claims |> List.ofArray);
            let existingClaims = List.map (fun claimToCheck -> (List.exists (fun decodedClaim -> decodedClaim = claimToCheck)  decodedClaimsList) claimsToCheck;
            let anyClaimsAbsent = List.exists (fun c -> c = false) existingClaims;
            match anyClaimsAbsent with
            | true -> NoSiga("JWT missing come claims")
            | false -> Siga(req)
            );;        
let isValid stri = 
  match stri with
  | "" -> None
  | _ -> Some(2)
        match jwtValOpt with
        | None -> NoSiga("JWT lookup fail - not in req. it needs the key 'jwtToken'")
        | Some(v) -> (
            let siVale = isValid(v)
            match v with
            | None -> NoSiga("Invalid JWT")
            | Some(_) -> Siga(req)
            );;            
    let validateJwt (req: Map<string, string>) =
        let jwtValOpt = getValFromKey req "jwtToken"
        match jwtValOpt with
        | None -> NoSiga("JWT lookup fail - not in req. it needs the key 'jwtToken'")
        | Some(v) -> (
            let siVale = isValid(v)
            match v with
            | None -> NoSiga("Invalid JWT")
            | Some(_) -> Siga(req)
            );;
    let checkJwtClaims (req: Map<string, string>) (claimsToCheck: string list) =
        let jwtValOpt = getValFromKey req "jwtToken"
        match jwtValOpt with
        | None -> NoSiga("JWT lookup fail - not in req. it needs the key 'jwtToken'")
        | Some(v) -> (
            let decodedClaimsList = decode<UserRights>(v) |> (fun x -> x.Claims |> List.ofArray);
            let existingClaims = List.map (fun claimToCheck -> (List.exists (fun decodedClaim -> decodedClaim = claimToCheck)  decodedClaimsList) claimsToCheck;
            let anyClaimsAbsent = List.exists (fun c -> c = false) existingClaims;
            match anyClaimsAbsent with
            | true -> NoSiga("JWT missing come claims")
            | false -> Siga(req)
            );;


    let getValFromKey (req: Map<string, string>) (k: string) =
        let lookup = req.ContainsKey(k)
        match k with
        | true -> Some(req.QueryStringParameters.[k])
        | false -> None ;;