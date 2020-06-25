module Cms
open System
open Microsoft.Security.Application // scope of AntiXss - access with Encoder.HtmlEncode()
open System.Text.RegularExpressions
open Json
open FSharp.AWS.DynamoDB
open Amazon.DynamoDBv2

type url = Window of string | Static of string;;
type monthVal = Month of string;;
type dayVal = Day of int;;
type structuredDate = monthVal * dayVal;;
let u = ("cha cha", Window("https://www.facebook.com/events/412337449396790/"));;
type location = City of string;;
type tour = TourName of string;;
type clubName = Club of string;;
type ticketLink = Ticket of url;;
type vipLink = Vip of url;;
type eventLink = EventLink of url;;
let oct15 = (
    EventLink(Window("https://www.facebook.com/events/412337449396790/")),
    (Month("October"), Day(15)),
    City("Vilnius, Lithuania"),
    Club("Kablys"),
    Ticket(Window("https://www.tiketa.lt/LT/shahmen_21862?fbclid=IwAR0TgYae6-9ZPeC319paQSV6R_fTla05es_aSN5ZiBQLeTMShJZmmrdfVSA")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-vilnius-tickets-69298863767")),
    TourName("Bless Tour")
);;
let verbatimXml = @"<book title=""Paradise Lost"">"


//[<HashKey>]
//    eventLink: string
//    [<RangeKey>]
type eventDto = {

    [<HashKey>]
    TourName: string    
    [<RangeKey>]
    EventLink: string
    // window, not static:
    EventLinkIsWindow: bool

    Month: string
    Day: int
    City: string
    Club: string
    TicketLink: string
    // window, not static:
    TicketLinkIsWindow: bool
    VipLink: string
    // window, not static:
    VipLinkIsWindow: bool
    };;

let tagEventData isWindow str sanitizeFn =
    match isWindow with
    | true -> EventLink(Window((sanitizeFn str)))
    | false -> EventLink(Static((sanitizeFn str)));;

let eventData (elink, _, _, _, _, _, _) sanitizeFn =
    match elink with
    | EventLink(Window(str)) -> (true, (sanitizeFn str))
    | EventLink(Static(str)) -> (false, (sanitizeFn str));;

//Encoder.HtmlEncode()
let tagTixData isWindow str sanitizeFn =
    match isWindow with
    | true -> Ticket(Window((sanitizeFn str)))
    | false -> Ticket(Static((sanitizeFn str)));;

let tixLinkData (_, _, _, _, tixLink, _, _) sanitizeFn =
    match tixLink with
    | Ticket(Window(str)) -> (true, (sanitizeFn str))
    | Ticket(Static(str)) -> (false, (sanitizeFn str));;

let tagVipData isWindow str sanitizeFn =
    match isWindow with
    | true -> Vip(Window((sanitizeFn str)))
    | false -> Vip(Static((sanitizeFn str)));;

let vipLinkData (_, _, _, _, _, vipLink, _) sanitizeFn =
    match vipLink with
    | Vip(Window(str)) -> (true, (sanitizeFn str))
    | Vip(Static(str)) -> (false, (sanitizeFn str));;



(* no Result wrapping needed below, this will never fail *)
(* a valid domain object (evt-tuple) ALWAYS yields a valid DTO object *)
let toDto evtTuple =
    let (evtLinkIsWindow, evtLinkStr) = (eventData evtTuple Encoder.HtmlEncode)
    let (vipLinkIsWindow, vipLinkStr) = (vipLinkData evtTuple Encoder.HtmlEncode)
    let (tixLinkIsWindow, tixLinkStr) = (tixLinkData evtTuple Encoder.HtmlEncode)
    let (_, (Month(monthStr), Day(dayInt)), City(cityStr), Club(clubStr), _, _, _) = evtTuple
    { EventLink = evtLinkStr;
      EventLinkIsWindow = evtLinkIsWindow;
      TicketLinkIsWindow = tixLinkIsWindow;
      VipLinkIsWindow = vipLinkIsWindow;
      City=cityStr;
      Club=clubStr;
      Day=dayInt;
      Month=monthStr;
      TourName="Bless Tour";
      TicketLink = tixLinkStr;
      VipLink = vipLinkStr;
      };;

(*return type below is a Result composed an of Event-tuple*)
(* it CAN yield an error, unline the FN above which can't because a valid domain obj always yields a valid DTO *)
(* how "valid"? via both typechecking, and validation predicate fns *)
(* we need more.... *)
(* and what about malfomed json, those errors? *)
let resultAccepting f =
    (fun arg ->
     match arg with
     | Ok(aVal) -> (f aVal)
     | Error(e) -> Error(e)
     );;
exception DtoError of string
(* SECURITY??? validate user-provided input AS EARLY AS POSSIBLE *)
let evtFromDto dto =
    let { City = cityStr; Club = clubStr; Day = dayInt; Month = monthStr; TicketLink = tixLinkStr; VipLink = vipLinkStr; TicketLinkIsWindow= tixLinkIsWindow; VipLinkIsWindow = vipLinkIsWindow; EventLinkIsWindow = evtLinkIsWindow; EventLink = evtLinkStr; TourName=t } = dto
    let tixLink = (tagTixData tixLinkIsWindow tixLinkStr Encoder.HtmlEncode)
    let vipLink = (tagVipData vipLinkIsWindow vipLinkStr Encoder.HtmlEncode)
    let evtLink = (tagEventData evtLinkIsWindow evtLinkStr Encoder.HtmlEncode)
    // (validate date... more code to come later (month, etc)...)
    let taggedDateTuple = (Month(monthStr), Day(dayInt))
    let taggedCity = City(cityStr)
    let taggedClub = Club(clubStr)
    match ((dayInt > 31) || (dayInt < 1)) with
    | true -> Error(DtoError("bad date"))
    | false -> Ok ((
            evtLink,
            taggedDateTuple,
            taggedCity,
            taggedClub,
            tixLink,
            vipLink,
            TourName(t)));;


let jsonFromEvent evt = 
    evt
    |> toDto
    |> Json.serialize;;

let dtoFromJson jsonStr =
    jsonStr
    |> Json.deserialize<eventDto>;;
let evtFromJson jsonStr =
    (dtoFromJson jsonStr)
    |> (resultAccepting evtFromDto);;
let testStr = """{"EventLink":"https://www.facebook.com/events/2356459534447042/","EventLinkIsWindow":true,"Month":"October","Day":23,"City":"London, UK","Club":"o2 Academy Islington","TicketLink":"http://ticketmaster-uk.tm7559.net/c/253158/431519/7559?u=https%3A%2F%2Fwww.ticketmaster.co.uk%2Fshahmen-london-10-23-2019%2Fevent%2F1F0056FF8B440D8D&fbclid=IwAR0af0jRMwt-vGwaJgj9OAeJ1AxfCicvc70diYAhkc8sg0YX7s3ZQVsbww8","TicketLinkIsWindow":true,"VipLink":"https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-london-tickets-69302679179","VipLinkIsWindow":true}""";;

let myDto = (dtoFromJson testStr);;
let myEvt = (evtFromJson testStr);;
let resultStr = (match myEvt with
                 | Ok(_) -> "OK"
                 | Error(_) -> "Nah");;
let dateOf evtTuple =
    match evtTuple with
    | (_,(Month(mstr), Day(dint)) ,_,_,_,_,_) -> mstr + " " + string(dint);;
let cityOf evtTuple =
    match evtTuple with
    | (_,_ ,City(cityStr),_,_,_,_) -> cityStr;;
let clubNameOf evtTuple =
    match evtTuple with
    | (_,_,_,Club(clubNombre),_,_,_) -> clubNombre;;
let ticketWindowUrl evtTuple =
    match evtTuple with
    | (_,_,_,_,Ticket(Window(urlStr)),_, _) -> urlStr
    | (_,_,_,_,Ticket(Static(urlStaticStr)),_,_) -> urlStaticStr;;
let vipUrl evtTuple =
    match evtTuple with
    | (_,_,_,_,_,Vip(Window(urlStr)),_) -> urlStr
    | (_,_,_,_,_,Vip(Static(urlStaticStr)),_) -> urlStaticStr;;
let eventUrl evtTuple =
    match evtTuple with
    | (EventLink(Window(urlStr)),_,_,_,_,_,_) -> urlStr
    | (EventLink(Static(urlStaticStr)),_,_,_,_,_,_) -> urlStaticStr;;
let stringifyForDsl s =  "\"" + s + "\"";; // don't like this "utility fn" -- needs to be more type aware. what string?


let serializeStr evtTuple =
    let thisEvtUrl = (eventUrl evtTuple)
    let thisVipUrl = (vipUrl evtTuple)
    let thisTixUrl = (ticketWindowUrl evtTuple)
    let thisClubname = (clubNameOf evtTuple)
    let thisDateStr = (dateOf evtTuple)
    let thisCity = (cityOf evtTuple)
    """
    (bind ((eventUrl 
    """
    + (stringifyForDsl thisEvtUrl) +
    """
    )
    (vipUrl 
    """
    + (stringifyForDsl thisVipUrl) +
    """
    )
    (tixUrl 
    """
    + (stringifyForDsl thisTixUrl) +
    """
    )
    (clubName 
    """
    + (stringifyForDsl thisClubname)  +
    """
    )
    (dateStr 
    """
    + (stringifyForDsl thisDateStr) +
    """
    )
    (cityStr 
    """
    + (stringifyForDsl thisCity) +
    """
    )
    ))
    """

let link urlVal content =
    match urlVal with
    | Window(st) -> """<a href="#" onclick="window.open('"""
                        + st +
                        """')">"""
                        + content +
                        """</a>"""
    | Static(st) -> @"<a href=""
                        + st +
                        @"">"
                        + content +
                        "</a>"
let button urlVal content = 
    """<button class="btn btn-dark" onclick="window.open('"""
    + urlVal +
    """')" type="button">""" 
    + content +
    """</button>"""

  
let block evtt = match evtt with
               | (EventLink(eL), _,_,_, Ticket(tL),Vip(vL),  TourName(_)) -> (
                     """<tr><th scope="row">"""
                        + (dateOf evtt) +
                        """</th><td>"""
                        + (link eL (cityOf evtt)) +
                        """</td><td>"""
                        + (link eL (eventUrl evtt)) +
                        """</td><td>"""
                        + (link eL (clubNameOf evtt)) +
                        """</td><td>"""
                        + (button (ticketWindowUrl evtt) "TI€KET$" ) +
                        """</td><td>"""
                        + (button (vipUrl evtt) "VIP" ) + 
                        """</td></tr>""" )

let oct16 = (
    EventLink(Window("https://www.facebook.com/events/644908719360796/")),
    (Month("October"), Day(16)),
    City("Kiev, Ukraine"),
    Club("Atlas"),
    Ticket(Window("https://concert.ua/event/shahmen?fbclid=IwAR0ZzFCxcCeMtbBTPUx5gsekymlLBGNRjsZb7uID3BkD9uNcFH0m9KQW1A4")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-kiev-tickets-69300067367")),
    TourName("Bless Tour")
);;
let oct18 = (
    EventLink(Window("https://vk.com/shahmen_mockba")),
    (Month("October"), Day(18)),
    City("Moscow, Russia"),
    Club("Moskva"),
    Ticket(Window("https://tele-club.ru/moskvaclub/shahmen-msk?utm_source=vk&utm_medium=post&utm_campaign=shahmen-msk&utm_term=anton-shibanov&utm_content=promoanons")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-moscow-tickets-69300773479")),
    TourName("Bless Tour")
);;
let uno0dto = (toDto oct18);;
let dtoConvertBackresult = (evtFromDto uno0dto);;
let uStr = match dtoConvertBackresult with
| Error(_) -> "ERR"
| _ -> "NO ERR";;
let oct19 = (
    EventLink(Window("https://vk.com/shahmen_vnvnc")),
    (Month("October"), Day(19)),
    City("St. Petersburg, Russia"),
    Club("VNVCN"),
    Ticket(Window("https://tele-club.ru/vnvnc/shahmen-vnvnc?utm_source=vk&utm_medium=post&utm_campaign=shahmen-vnvnc&utm_term=anton-shibanov&utm_content=promoanons")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-st-petersburg-tickets-69301618005")),
    TourName("Bless Tour")
);;


let oct23 = (
    EventLink(Window("https://www.facebook.com/events/2356459534447042/")),
    (Month("October"), Day(23)),
    City("London, UK"),
    Club("o2 Academy Islington"),
    Ticket(Window("http://ticketmaster-uk.tm7559.net/c/253158/431519/7559?u=https%3A%2F%2Fwww.ticketmaster.co.uk%2Fshahmen-london-10-23-2019%2Fevent%2F1F0056FF8B440D8D&fbclid=IwAR0af0jRMwt-vGwaJgj9OAeJ1AxfCicvc70diYAhkc8sg0YX7s3ZQVsbww8")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-london-tickets-69302679179")),
    TourName("Bless Tour")
);;


let oct25 = (
    EventLink(Window("https://www.facebook.com/events/1258273497629713/")),
    (Month("October"), Day(25)),
    City("Berlin, Germany"),
    Club("Burg Schnabel"),
    Ticket(Window("https://www.eventbrite.co.uk/e/shahmen-2510-berlin-burg-schnabel-tickets-69211532557?aff=efbeventtix&fbclid=IwAR1JDqIdrc1GMmzv1cp8vXmUFK0FyC0G9tLzGxE9hNgK6xD7EdKpDWp3mww")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-berlin-tickets-69303278973")),
    TourName("Bless Tour")
);;

let oct31 = (
    EventLink(Window("https://www.facebook.com/events/616113312130362/")),
    (Month("October"), Day(31)),
    City("Warsaw, Poland"),
    Club("Hydrozagadka"),
    Ticket(Window("https://www.followthestep.com/pl/buy/2145886?fbclid=IwAR3gAAHNgPY9L_R0bLxSimUE-V1wkNNrTaG7JRp8ovB6MzaKpHpB5Iw8Fkg")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-warsaw-tickets-69304871737")),
    TourName("Bless Tour")
);;

let nov1 = (
    EventLink(Window("https://www.facebook.com/events/1258273497629713/")),
    (Month("November"), Day(1)),
    City("Prague, Czech Republic"),
    Club("Cross Club"),
    Ticket(Window("https://goout.net/cs/listky/shahmen/ajcf/?fbclid=IwAR1C6ggUFQzDbvuipUtHfC0TUEXkNAfIWOFZ5ZTcLSurbjbLRFz7odWvM4o")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-prague-tickets-69305501621")),
    TourName("Bless Tour")
);;

let nov3 = (
    EventLink(Window("https://www.facebook.com/events/1258273497629713/")),
    (Month("November"), Day(3)),
    City("Budapest, Hungary"),
    Club("Durer Kert"),
    Ticket(Window("https://tixa.hu/shahmen_durer2019?fbclid=IwAR0n__mzQFCSPOBZ2uAt-kf5vHdAGfnAjswpYzlZXM083XHUUStYwpDYqSU")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-budapest-tickets-69306275937")),
    TourName("Bless Tour")
);;

let nov6 = (
    EventLink(Window("https://www.facebook.com/events/416138199018719/")),
    (Month("November"), Day(6)),
    City("Cluj Napoca, Romania"),
    Club("Form Space"),
    Ticket(Window("https://www.iabilet.ro/bilete-shahmen-at-form-space-45197/?fbclid=IwAR2Dv-s9Yjnp76fjAa5VBNhY2BCzZwq3v54ffKKhTgxtVnNUTXgMHH9vKBU")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-cluj-tickets-69307034205")),
    TourName("Bless Tour")
);;

let nov7 = (
    EventLink(Window("https://www.facebook.com/events/2197812673662587/")),
    (Month("November"), Day(7)),
    City("Bucharest, Romania"),
    Club("Deschis Atelier"),
    Ticket(Window("http://thefresh.ro/shahmenpresbythefresh/?fbclid=IwAR35eeNqblbVCb3KVW7XEK9acYzNUOSOqyigxRTVpeYbGPycy8pxKoszBfY")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-bucharest-tickets-69308867689")),
    TourName("Bless Tour")
);;


let nov9 = (
    EventLink(Window("https://www.facebook.com/events/946416325698733/")),
    (Month("November"), Day(9)),
    City("Thessaloniki, Greece"),
    Club("WE"),
    Ticket(Window("https://www.viva.gr/tickets/music/we-polychoros/shahmen-live-in-thessaloniki/?fbclid=IwAR2Qn3mmWj2KQW0olv2QEtSvdWSbffoRb-I-bvyX1CUDq1MkeSAGd_tl1bg")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-thessaloniki-tickets-69310051229")),
    TourName("Bless Tour")
);;

let nov20a = (
    EventLink(Window("https://www.facebook.com/events/377106316555199/")),
    (Month("November"), Day(20)),
    City("Oslo, Norway - U 18"),
    Club("Bla"),
    Ticket(Window("https://www.ticketmaster.no/event/616647")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-oslo-tickets-69310681113")),
    TourName("Bless Tour")
);;
let nov20b = (
    EventLink(Window("https://www.facebook.com/events/377106316555199/")),
    (Month("November"), Day(20)),
    City("Oslo, Norway - 18+"),
    Club("Bla"),
    Ticket(Window("https://www.ticketmaster.no/event/616653")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-oslo-tickets-69310681113")),
    TourName("Bless Tour")
);;

let nov23 = (
    EventLink(Window("https://www.facebook.com/events/507611133328250/")),
    (Month("November"), Day(23)),
    City("Helsinki, Finland"),
    Club("Korjaamo"),
    Ticket(Window("https://www.tiketti.fi/shahmen-usa-nld-kulttuuritehdas-korjaamo-vaunusali-helsinki-lippuja/63950?fbclid=IwAR3tCcdQbm7dLcKCk_o0PrSCQEDS9kI_TLkoOyWKJGy2s-Lf8OZjpPAWtiE")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-helsinki-tickets-69311315009")),
    TourName("Bless Tour")
);;

let nov24 = (
    EventLink(Window("https://www.facebook.com/events/672636426536690/?active_tab=about")),
    (Month("November"), Day(24)),
    City("Amsterdam, Netherlands"),
    Club("Melkweg"),
    Ticket(Window("https://www.ticketmaster.nl/event/245339?brand=nl_melkweg&fbclid=IwAR3Lz6zybhurZXGfnxklVRnxS1UkbByQGUNshoM64kPPWL0lT85qHhpAnvY")),
    Vip(Window("https://www.eventbrite.co.uk/e/shahmen-vip-meet-greet-amsterdam-tickets-69311676089")),
    TourName("Bless Tour")
);;


let datez = oct15 :: oct16 :: oct18 :: oct19 :: oct23 :: oct25 :: oct31 :: nov1 :: nov3 :: nov6 :: nov7 :: nov9 :: nov20a :: nov20b :: nov23 :: nov24 ::  [];;
let transformed = datez |> List.map dateOf;;
let ticketWindowNoHaterz = datez |> List.map ticketWindowUrl;;



let optNew = ((fun lst -> 
    match lst with
    | [] -> ""      // we tried None
    | h :: t -> h   // we tried Some(h)
) transformed);;

let optNew2 = ((fun lst -> 
    match lst with
    | [] -> ""      // we tried None
    | h :: t -> h   // we tried Some(h)
) ticketWindowNoHaterz);;
// DRY -- this anon fn needs a name!!

let uuuu = block oct15;;
let blocks = datez |> List.map block |> String.concat("");;
let blocks2 = "<table class=\"table table-dark\">" + blocks + "</table>";;
let mySerializedMap = datez |> List.map serializeStr |> String.concat " :: "
let mySerializedMap2 = mySerializedMap + " :: [] "
let Window(st) = u
let getStr win =
    match win with 
    | Window(str) -> str
    | _ -> "Nada"
let (a,b) = u;;
//let nn = getStr(b);;
let nn = dateOf oct15;;
let nn2 = dateOf oct16;;
let nn3 = optNew;;
let nn4 = optNew2;;
let nnnn0 = uuuu;;
let n0n0 = mySerializedMap2;;
let finalForm = "(u-quote (:serialized-bind:) "  + n0n0 +  ") ";;    
let demo = datez |> List.map jsonFromEvent |> String.concat ",";;
let bracketsDemo =  "[" + demo + "]";;
let tst =  (jsonFromEvent oct23);;