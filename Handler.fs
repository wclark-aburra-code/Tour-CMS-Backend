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
//type DbEventRecord = {
//    [<HashKey>]
//    eventLink: string
//    [<RangeKey>]
//    day: int
//    dto: Cms.eventDto option
//} // migrate -- we want a sort order
// yo - we need a User record to be saved
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
    //let table = TableContext.Create<DbEventRecord>(client, tableName = "eventsTable", createIfNotExists = true)
    let table = TableContext.Create<Cms.eventDto>(client, tableName = "properDtos", createIfNotExists = true)
    //let testDbRecord = {eventLink= "yo"; day=22; dto=None}
    //let oct15dto = (toDto Cms.oct18)    
    //let jsonTest = (Cms.jsonFromEvent Cms.oct18)
    //let testDbRecord2 = {eventLink= "yo"; day=22; dto=Some(oct15dto)}
    //let testDbRecord3 = {eventLink= "yo"; day=13; dto=None}
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

    //let idx =
    //    let mutable i = 1 // as ref? difference?
    //    fun () ->
    //        i <- i + 1
    //        i
    //let factory = 
    //    let counter = ref 0
    //    fun () -> 
    //        counter.Value <- !counter + 1
    //        !counter            
    // so encoded JWT is eyJhbGciOiJBMjU2S1ciLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0.L06g5LB2SK39pjnko0utGqz1Xk_fgVSZn31zP-MPGHgxOZZ2wyZ5EZuU_sMrseukRkVJh0tlrmkM8tgKFvm6DCfH9PaqTXQD.3I9TLTCG4NOot9R33voTrA.b8h7N7UiRMJpJbnpai1habVExfdAYHAXrrWq7Y_s4mUle02FR6RnyZJS70M8w4_CJ68dw9qGctxLiKYvLwwqTezwZ8zhWBm5-TPf-08kOhY.9DJxszNDfevjd_vyuafpLky2klLFfy6CqUuG0afEnTo
    // we will need a params check middleware for sure!
    // YO when it errors out due to this, we lose the headers, and it appears as a CORS error!
    let makeMap (request:APIGatewayProxyRequest) contenido =
        //let body = request.Body  // this is AntiXss! finally!
        let toke = request.Headers.["Authorization"] // VALIDATE??? ERR CHECK
        {Token = toke; Content = contenido; Dtos = []; Headers = Map.empty }    
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
        //let reqPostMw = (applyMw newReq [validateJwt2; (checkJwtClaims3 ["gangsta"; "pimp"])])
        //let payload = {
        //    Issuer = "my.dude"; Expiry = 1300819380; Claims = [|"gangsta"; "pimp"|];
        //}
        //let listaType = request.QueryStringParameters.["dtoList"].GetType()
        //let listaType = "adsf"
        //let toke = request.QueryStringParameters.["Auth"] // need a TRY for all params -- MW!
        //let toke = request.Headers.["Authorization"]
        //let reqMap = {Token = toke; Content = Get(Map.empty); Dtos = []; Headers = Map.empty }
        //let reqMap = (makeMap request (Get(Map.empty)))
        (makeMap request (Put(request.Body)))
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "pimp"])])
            |> (respondedor respondTemplateValid)
    let hello(request:APIGatewayProxyRequest) =
        //let base64StrToke = request.QueryStringParameters.["Authorization"] + "="
        //let decodedTokeBytes = (from64 base64StrToke)
        //let newReq = {Headers = Map.empty.Add("Authorization", decodedTokeBytes); QueryStringParameters = Map.empty;}
        //let reqPostMw = (applyMw newReq [validateJwt2; (checkJwtClaims3 ["gangsta"; "pimp"])])
        //let payload = {
        //    Issuer = "my.dude"; Expiry = 1300819380; Claims = [|"gangsta"; "pimp"|];
        //}
        //let listaType = request.QueryStringParameters.["dtoList"].GetType()
        //let listaType = "adsf"
        //let toke = request.QueryStringParameters.["Auth"] // need a TRY for all params -- MW!
        //let toke = request.Headers.["Authorization"]
        //let reqMap = {Token = toke; Content = Get(Map.empty); Dtos = []; Headers = Map.empty }
        //let reqMap = (makeMap request (Get(Map.empty)))
        (makeMap request (Get(Map.empty.Add("TourName", request.QueryStringParameters.["TourName"] ))))
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "pimp"])])
            |> (respondedor respondHelloValid)
        //let gotit = (match (isValid toke) with
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
        //let toke = request.Headers.["Authorization"]
        //let reqMap = {Token = toke; Content = Put(body); Dtos = []; Headers = Map.empty }
        (makeMap request (Put(request.Body))) // it's POST!!!!
            |> (applyMw [validateJwt2; (checkJwtClaims3 ["gangsta"; "pimp"])])
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


        // ***MAIN THANGS***

        // JTI/expiry/etc, OTP!!! blacklist too (refresh token?)
        // 
        // *DONE*templating DSL basics (not yet circular-recursive, just window link function... show usefulness of type safety + composition in this case)
        // *DONE* "new window" checkbox
        // *DONE* selectbox of associated Tour Name
        // *DONE* selectbox-atom GET, refactor get - DRY/boilerplate
        // *DONE* swap select hides edit-view if active from swapped-out tour
        // cache get response per hash-key (tour name)

        // ***MAIN THANGS***
        // fuller html template
        // make sure we don't have a bug around the PUT -- it is overwriting, no?
        // bug -- only one result written?
        // add mw for validating keys, and also a static checker for that!
        // tab order
        // spinnaz (results fade in/out)
        // scroll
        // "headless - endpoints, json/rt"

        // spinner (+ fade?)
        // dynamic list for select box
        // zero case?


        // map optional "try" is a must

        // TDD w pure functions
        // CLJS STATE as closure -- so the "inner function" can be tested!!!! it's predictable
        // curry multiple states used as "russian doll" closure (REVIEW russian doll cache btw!)
        // when testing, instead of state, use const!! it's the same damn thang!
        // better yet, could have a dynamic stub w new values keep getting consed
        // REVIEW -- can my optimized hash for U be memoized? how is this "collapsing sets, coalescing sets, w/ specificity increasing uniformly" deal going to work out w/ "memoize" hash? turning assoc hashlike (pure) to hash (impure, imperative, rnd access / hash fn)

        // macro!!!!!!!!!

        // THEN
        // config cleanup... blacklist (env vars)

        // then
        // no need 2 req again on switch-tour, we've locally updated state

        // THEN
        // db layer interface, incl ignore right?
        // blacklist, login (hmac pwd? refresh token?)
        // openfaas refactor, for watchdog/imperative OR response-returned/functional (variant of fn types?)
        
        // THEN
        // needed-keys/try-params middleware (static analysis?????), zero case, errors catch, cleanup
        // json schema        
                    // JSON schema validator in SLS/cloudform-yaml (and "models"? registered w/ api gateway? how much of that do we need filled out?)
                        // https://www.serverless.com/framework/docs/providers/aws/events/apigateway/
                        // see "request schema validators" (also request templates, etc for response too, all api gateway config via cloudformation yaml)
        // *DONE* FIX container data structure, fuck dat shit, def no option dto... pk??

//THEN
        // generics for these interfaces
        // automate S3 table name? (create if not, etc)
        // abstraction -- a *TAG* for Respondedor type function? interface can be helped by tagging fn types + pattern matching against them
// BOOM

        // *DONE* WE NEED a full token
        // *DONE* WE HAVE a key, not a token. that's what we keep using
        // *DONE* gotta reverse course and generate a token, then use that in Auth


        // *DONE* JSON  deserialize (type provider?), final DTO logic
        // *DONE* GET dates 
        // *DONE* PUT updated dates
        // *DONE* Client side POST API call
        // *DONE* Client side GET API call
        // *DONE* JWT from URL (window.??)
        // *DONE*Middeware reorganization, full optional-composed translation, async version
        // *DONE* (it's encapsulated in dto logic) XSS encode middleware for post, each querystring param (map)
        // *DONE* CLJ data-record structure


        // empty list submitted edge case



        // *DONE* partition key is the tour
        // *DONE* sort key is url or something
        // we load one tour at a time, as per a select box ... maybe don't need tours table, it's just the name/year
        // some rbac?
        // *DONE*  advanced dynamodb
        // *DONE* no more of this ID-gen in the state... do we need to gen ids? i think not... review Dynamo!

        //  *DONE* delete item -- need a Key to be created!
        // https://docs.aws.amazon.com/sdkfornet/v3/apidocs/items/DynamoDBv2/MDynamoDBDeleteItemDeleteItemRequest.html

        //  *DONE*yo delete should happen first


        // *DONE* borrar? the POST body needs to be built with a set of IDs to be deleted
        //       undo later
        //  *DONE*   // bug: changing month changes partition/PK and therefore we may not get a proper update, but duplication
        //  *DONE* deletion? we got no mem, just have a larger, multi-transactional "dto" that gets posted
        //  *DONE* we really want put to be straight-forward, and for the delete case to be handled MAD automatically



        // *DONE* S3 site gen - F# api for that, diff AWS role/permission though for this than the keyz        






// THEN
        // typesafe templating dsl with blob/list/etc and recursive containment )mut recur types/fns, metacirc_

        // *DONE* refactor and rm boilerplate (more functional abstractions/DRY) for Responder functions
        // *DONE*  make them a predictable sort of type w/ an extra wrapper fn to rm duplicated logic

        // THEN (cleanup basically)
        // global "headers"+ defaults, claims list, etc    

        // async middleware nested map WAY later, use JSON deserialize for now and ???trust the input???
        
        // username/pwd login (email? cognito way later... MFA enabled when it comes to that)
        // generics? other meta? code exp?

        // 

        // validations, including client side (that will kill an entry, stop them pre-submit via regex)
        // no valid JWT case (400?), animate? animate wait? opaque overlay?
        // UI date fields, autofill
        // Config MGMT including blacklist, ops (S3 bucket protected by a unique role, no?)
            // https://www.serverless.com/blog/serverless-secrets-api-keys/
        // JTI/EXP in JWT
        // CloudFormation static-site deployment (SSL? Http2? CORS?)
        // autogen mad keys, get used to blacklisting/etc
        // zero case
        // client-side error handling (response status, no jwt, etc)


        // anti dynamodb injection


        ///THEN
        // sort/etc -- actual domain logic yo -- gotta be date internals
        // more templating w/ Window logic/etc
            // later we'll have actual templating
        // client-side date sort
        
        // extra messaging ("new item added", "u gotta save before post", etc)
        // more animation -- form load, etc (spinner?)
        // colors, styles, extra font

        
        // NoSiga composed of ADT type (almost just an enum, possibly w/ string composition)
        // YA!

        // *IFF there is time*

        // what else to validate/escape user input??? what's full Node.JS approach? htmlentities plus dynamo alarms plus what?
        // do we need a map for that???

        // generic function type signatures for non sls?
        // regional ops, Dynamo needs to be better optimized
        // frontend libs (js typesafe api, no QL), templating
        // rt - sns, connectionless push, mobile
        //          channels, persistence, non-kfk? more like serverless -- for a small business, it can "sleep"
        //          media, rtc, incl usb-via-browser
        // canvas enabled, shared canvas, in rtc too
        // globe/geo enabled
        // deep sec, mfa, roles, abac in dope ui, simple role mgmt -- a "tree" or "hand" (discourage sub-sub-sub roles, too many superuser permissions)
        // sso integration, good oauth
        // final (sync) middleware sig
        // we take (token/str option, GET(shallow string map) | PUT (formData str)) 
        // MW validates/encodes/sanitizes formData and queryParams
        // MW packs token into that field from formData or queryParams
        // MW takes GET or PUT arg to see method, know where to look (queryParams/formData) for data/token


        // refactor CSS (sass? responsive selectors? diff media queries? golden ratio? more font strategy...)
        //          css-grid? flexbox?
        //          Maya, wasm, more assembly
        //          more opacity, animation
        //          on top of Maya, more SVG animation

