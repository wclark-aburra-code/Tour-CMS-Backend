namespace AwsDotnetFsharp
open Json
open Amazon  
open Amazon.S3  
open Amazon.S3.Model  
//open Amazon.S3.IO

open Amazon.Lambda.Core
open FSharp.AWS.DynamoDB
open Amazon.DynamoDBv2
open Amazon.Lambda.APIGatewayEvents
open Cms
open Jose
open System.Text.RegularExpressions
open FSharp.Data
open System.IO
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Security.Cryptography
open System.Web

open Microsoft.Security.Application
//open HtmlExtensions

[<assembly:LambdaSerializer(typeof<Amazon.Lambda.Serialization.Json.JsonSerializer>)>]
do ()

type UserRights = 
    { Issuer : string; Expiry: int; Claims: string[] }
type RequestContent = 
    | Get of Map<string, string>
    | Post of string
    | Put of string
    | Delete of (string*string) list // 2-part PK in Dynamo
    ;;
type MappedAPIGatewayProxyRequest = {Token: string; Content: RequestContent; Dtos: Cms.eventDto list; Headers: Map<string, string> }
type JsonResponse = 
    { Dates: Cms.eventDto[]}    
type middlewareResult = Siga of MappedAPIGatewayProxyRequest | NoSiga of string;; // NoSiga err eventualls    

module Handler =
    open System
    open System.IO
    open System.Text
    let headerz = dict [ "Content-Type", "application/json";"Access-Control-Allow-Origin", "*"; ]
    let noSigaAccepting middlewareFn =                                                
        (fun preed ->
            match preed with
            | NoSiga(x) -> NoSiga(x)
            | Siga(y) -> (middlewareFn y))
    let s3Client = new AmazonS3Client()
    let rec applyMw mwList (req: MappedAPIGatewayProxyRequest) =
        (match mwList with
            | last :: [] -> (
                match ((noSigaAccepting last) (Siga(req))) with 
                | NoSiga(x) -> NoSiga(x)
                | Siga(x) -> Siga(x)
                )
            | h :: t -> (
                match ((noSigaAccepting h) (Siga(req))) with 
                | NoSiga(x) -> NoSiga(x)
                | Siga(x) -> (applyMw t req)
                ))

    exception DynamoError of string
    let client = new AmazonDynamoDBClient()
    let table = TableContext.Create<Cms.eventDto>(client, tableName = "properDtos", createIfNotExists = true)
    let private createPassPhrase() = 
        let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
        let randomNumber = Array.init 32 byte
        crypto.GetBytes(randomNumber)
        randomNumber
    let base64Str = "wayKFUD65TtsjQ96Nslpk07YLmWz2pStJKTNEqttkRE="
    let private from64 in64 = 
      (System.Convert.FromBase64String in64)
    let decodedBytes = (from64 base64Str)
    let u = decodedBytes
    
    let private passPhrase =
        let encoding = Encoding.UTF8
        "rmb352BMGQ#"  |> System.Text.Encoding.ASCII.GetBytes
    let private encodeString (payload:string) =
        Jose.JWT.Encode(payload, u, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

    let private decodeString (jwt:string) =
        // To make this secure a key should generated and used as a pass phrase with the encoding below
        Jose.JWT.Decode(jwt, u, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)
    
    let encode token =
        JsonConvert.SerializeObject token
        |> encodeString
    let decode<'a> (jwt:string) : 'a =
        decodeString jwt
        |> JsonConvert.DeserializeObject<'a>

    let isValid (jwt:string) : UserRights option =
        try
            let token = decode jwt
            Some token
        with
        | _ -> None
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        (match m.Success with
            | true -> Some(List.tail [ for g in m.Groups -> g.Value ])
            | false -> None )
    
    let validateJwt2 (req: MappedAPIGatewayProxyRequest) =
        let authToken = req.Token
        match isValid(authToken) with
            | Some(_) -> Siga(req)
            | None -> NoSiga("invalid jwt")
        
    let checkJwtClaims3 (claimsToCheck: string list) (req: MappedAPIGatewayProxyRequest)  =
        let authToken = req.Token
        let decodedClaimsList = decode<UserRights>(authToken) |> (fun x -> x.Claims |> List.ofArray);
        let existingClaims = (List.map (fun claimToCheck ->     (List.exists (fun decodedClaim -> decodedClaim = claimToCheck)  decodedClaimsList)        ) claimsToCheck)
        let anyClaimsAbsent = List.exists (fun c -> c = false) existingClaims;
        match anyClaimsAbsent with
        | true -> NoSiga("JWT missing come claims")
        | false -> Siga(req)


    let makeMap (request:APIGatewayProxyRequest) contenido =
        //let body = request.Body  // this is AntiXss! finally!
        let tokenn = request.Headers.["Authorization"] // VALIDATE??? ERR CHECK
        {Token = tokenn; Content = contenido; Dtos = []; Headers = Map.empty }    
    let respondHelloValid (rq: MappedAPIGatewayProxyRequest) =
        (
            let {Token=_; Content=content; Dtos=_; Headers=_ } = rq
            let jsonArray =
                //table.Scan 
                (match content with 
                | Get(body) -> table.Query(keyCondition = <@ fun r -> r.TourName = body.["TourName"] @>)
                | _ -> table.Scan() ) // if no tourname, just get them all
                //|> (Seq.map (fun dto -> (Json.serialize dto) ))
                //|> (Seq.map (fun (dbRecord) ->
                //        (match dbRecord with
                //        | {eventLink = _; day = _; dto = Some(myRecord)} -> myRecord
                //        | {eventLink = _; day = _; dto = None} -> oct15dto // MUST FIX
                //        // BTW -- how does a compiler check this, or tuple-combination types, exhaustively?
                //        )
                //    ))
                |> Array.ofSeq
            let serializedBody = Json.serialize {Dates = jsonArray}
            APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = serializedBody)
        )
    exception InnerError
    let strip st =
        st |> Seq.toList |> (List.filter (fun c -> c <> ' ')) |> List.map (fun x -> string(x)) |> (List.fold (+) "")
    let respondTemplateValid (rq: MappedAPIGatewayProxyRequest) =
        (
            let {Token=_; Content=content; Dtos=_; Headers=_ } = rq
            let bodyStr = (match content with
                | Put(b) -> b
                | _ -> ""
            )
            let parsed = JObject.Parse(bodyStr)
            let tourName = string(parsed.["TourName"])  // NEED A TYPESAFE GUARD here that does 400/Err/NOSIGA
            let recordz = table.Query(keyCondition = <@ fun r -> r.TourName = tourName @>) |> List.ofSeq
            let htmlList = recordz |> List.map (fun rc ->  rc |> Cms.evtFromDto)
            let passed = htmlList |> List.filter (fun dt -> (match dt with 
            | Ok (_) -> true
            | _ -> false
            ))
            let htmlStrs = passed |> List.map (fun rs -> (
                match rs with
                | Ok(x) -> (Cms.block x)
                | _ -> raise InnerError
            ))
            let joined = htmlStrs |> (List.fold (+) "")   // STRING-LIST *JOIN* -- encapsulate in reusable function
            let fname = "site-" + strip(tourName) + ".html"
            let s3Req = new PutObjectRequest(BucketName = "tour-site-thangs",
                                            Key = fname, 
                                           ContentBody = joined)
            (( s3Client.PutObjectAsync(s3Req)) |> ignore )
            APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "we good!")
        )        
    let respondedor respondValid (req: middlewareResult) =
        (match req with
            | NoSiga(msg) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 400, Body=string("nah u ain't good"))
            //|   Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "***type***" + string(listaType) ))
            //|   Siga(rq) -> (respondHelloValid rq))
            |   Siga(rq) -> (respondValid rq))
    //let respondHello (req: middlewareResult) =
    //    (match req with
    //        | NoSiga(msg) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 400, Body=string("nah u ain't good"))
    //        //|   Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "***type***" + string(listaType) ))
    //        |   Siga(rq) -> (respondHelloValid rq))
    let template(request:APIGatewayProxyRequest) =
        //let base64StrToke = request.QueryStringParameters.["Authorization"] + "="
        //let decodedTokeBytes = (from64 base64StrToke)
        //let newReq = {Headers = Map.empty.Add("Authorization", decodedTokeBytes); QueryStringParameters = Map.empty;}
        //let reqPostMw = (applyMw newReq [validateJwt2; (checkJwtClaims3 ["gangsta"; "don"])])
        //let payload = {
        //    Issuer = "my.dude"; Expiry = 1300819380; Claims = [|"gangsta"; "don"|];
        //}
        //let listaType = request.QueryStringParameters.["dtoList"].GetType()
        //let listaType = "adsf"
        //let tokenn = request.QueryStringParameters.["Auth"] // need a TRY for all params -- MW!
        //let tokenn = request.Headers.["Authorization"]
        //let reqMap = {Token = toke; Content = Get(Map.empty); Dtos = []; Headers = Map.empty }
        //let reqMap = (makeMap request (Get(Map.empty)))
        (makeMap request (Put(request.Body)))
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "don"])])
            |> (respondedor respondTemplateValid)
    let hello(request:APIGatewayProxyRequest) =
        //let base64StrToken = request.QueryStringParameters.["Authorization"] + "="
        //let decodedTokeBytes = (from64 base64StrToken)
        //let newReq = {Headers = Map.empty.Add("Authorization", decodedTokeBytes); QueryStringParameters = Map.empty;}
        //let reqPostMw = (applyMw newReq [validateJwt2; (checkJwtClaims3 ["gangsta"; "don"])])
        //let payload = {
        //    Issuer = "my.dude"; Expiry = 1300819380; Claims = [|"gangsta"; "don"|];
        //}
        //let listaType = request.QueryStringParameters.["dtoList"].GetType()
        //let listaType = "adsf"
        //let tokenn = request.QueryStringParameters.["Auth"] // need a TRY for all params -- MW!
        //let tokenn = request.Headers.["Authorization"]
        //let reqMap = {Token = tokenn; Content = Get(Map.empty); Dtos = []; Headers = Map.empty }
        //let reqMap = (makeMap request (Get(Map.empty)))
        (makeMap request (Get(Map.empty.Add("TourName", request.QueryStringParameters.["TourName"] ))))
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "don"])])
            |> (respondedor respondHelloValid)
        //let gotit = (match (isValid tokenn) with
        //                | Some(_) -> "T"
        //                | None -> "fail, fool"
        //)
        //let encoded = encode(payload)
        //let decoded = decode<UserRights>(encoded) |> (fun x -> x.Claims |> List.ofArray) |> (List.fold (+) "")
        
        //"Access-Control-Allow-Methods", "GET"; 
        //"Access-Control-Allow-Headers", "Origin, Access-Control-Request-Method, Access-Control-Request-Headers"
        
        //let jsonList =
        //    table.Scan()
        //    //|> (Seq.map (fun dto -> (Json.serialize dto) ))
        //    |> (Seq.map (fun (dbRecord) ->
        //            (match dbRecord with
        //            | {eventLink = _; day = _; dto = Some(myRecord)} -> myRecord
        //            | {eventLink = _; day = _; dto = None} -> oct15dto // MUST FIX
        //            // BTW -- how does a compiler check this, or tuple-combination types, exhaustively?
        //            )
        //        ))
        //    |> Array.ofSeq
        //let serializedBody = Json.serialize {Dates = jsonList}
        // Token: string; Content: RequestContent; Dtos: Cms.eventDto list; Headers: Map<string, string>
        //(match Siga({Token=""; Headers=Map.empty; QueryStringParameters=Map.empty }) with
        //    | NoSiga(msg) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 400, Body=string("yeah u good"))
        //    //|   Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "***type***" + string(listaType) ))
        //    |   Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = serializedBody))
        //    //| Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = string(encoded) + "**yeah**" + decoded + "**whoa**" + string(Convert.ToBase64String(u) + "***YO***" + gotit )))
    let respondNuevoValid (rq: MappedAPIGatewayProxyRequest) = // WHAT DOES this look like for an empty list submitted?
        match rq with
            |  {Token = _; Content = Put(body); Dtos = _; Headers = _ } -> 
                (
                    let parsed = JObject.Parse(body)
                    let dtoParseResults = (parsed.["dates"] :?> JArray) |> (Seq.map (fun jObj -> (match (jObj.ToString() |> Cms.dtoFromJson) with
                        //| Ok(dtoVal) ->   {eventLink= "yo"; day=factory(); dto=Some(dtoVal)} 
                        | Ok(dtoVal) -> dtoVal
                        //| Error(_) ->   {eventLink= "yo"; day=1; dto=None}             
                        | Error(_) -> raise (DynamoError("parse error")) // # option/result/siga pater
                    )))
                    
                    let newLinx = dtoParseResults |> (Seq.map (fun {EventLink=eventVinculo} -> eventVinculo  )) |> List.ofSeq
                    let updatedDeletions = (parsed.["orig-keys"] :?> JArray) |> (Seq.map (fun sObj -> (string(parsed.["tour-name"]), sObj.ToString()))) |> (Seq.filter (fun (_,sStr) ->  not (List.exists (fun link -> link = sStr)  newLinx ) )) |> List.ofSeq
                    let deletions = (parsed.["to-delete"] :?> JArray)  |> (Seq.map (fun evtLinkStr -> (string(parsed.["tour-name"]), string(evtLinkStr)))) |> List.ofSeq
                    (updatedDeletions @ deletions) |> List.iter (fun item -> (table.DeleteItem(TableKey.Combined(item)) |> ignore)  )      

                    dtoParseResults |> Seq.iter (fun item -> (table.PutItem(item) |> ignore)  )
                    
                    APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "Ya!")
                )
            | _ -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 400, Body=string("nah u ain't NO good"))    
    //let respondNuevo (req: middlewareResult) =
    //    (match req with
    //        | NoSiga(msg) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 400, Body=string("nah u ain't good"))
    //        //|   Siga(rq) -> APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = "***type***" + string(listaType) ))
    //        | Siga(r) -> (respondHelloValid r) )

    

    let nuevo(request:APIGatewayProxyRequest) =
        //let body = request.Body  // this is AntiXss! finally!
        //let tokenn = request.Headers.["Authorization"]
        //let reqMap = {Token = tokenn; Content = Put(body); Dtos = []; Headers = Map.empty }
        (makeMap request (Put(request.Body))) // it's POST!!!!
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "don"])])
            |> (respondedor respondNuevoValid)
        //(respondNuevo mwResult)
        
        // ^ this won't work -- escapes JSON quotes. we gotta inject this into the JSON parser
        //let myAuth = request.Headers.["Authorization"] // try key, MW etc
        //let parsed = JObject.Parse(body)
        //let eventParseResults = (parsed.["dates"] :?> JArray) |> (Seq.map (fun jObj -> jObj.ToString() |> Cms.evtFromJson ))
        //let dtoParseResults = (parsed.["dates"] :?> JArray) |> (Seq.map (fun jObj -> (match (jObj.ToString() |> Cms.dtoFromJson) with
        //            | Ok(dtoVal) ->   {eventLink= "yo"; day=factory(); dto=Some(dtoVal)} 
        //            | Error(_) ->   {eventLink= "yo"; day=1; dto=None}             
        //    )))
        //dtoParseResults |> Seq.iter (fun item -> (table.PutItem(item) |> ignore)  )
        //let key : TableKey = table.PutItem(testDbRecord2)
        //let key2 : TableKey = table.PutItem(testDbRecord3)
        //let scFirst =
        //    table.Scan()
        //    |> Array.toList
        //    |> List.tryHead
        //let headerz = dict [ "Content-Type", "application/json";"Access-Control-Allow-Origin", "*"; ]            
        //let res = (match scFirst with
        //                | None -> "No resulta!"
        //                | Some(s) -> (string s.day))
        //APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = myAuth)
        //APIGatewayProxyResponse(Headers = headerz, StatusCode = 200, Body = body)


