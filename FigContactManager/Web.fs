module FigContactManager.Web

open System
open System.Web
open System.Web.Routing
open System.Web.Mvc
open Figment
open WingBeats
open WingBeats.Xml
open WingBeats.Xhtml
open global.Formlets
open FigContactManager.Model
open WingBeats.Formlets
open System.Xml.Linq

type WebGetRoute =
    | AllContacts
    | NewContact
    | EditContact of int64
    | AllGroups
    | Error of string

type WebPostRoute =
    | DeleteContact
    | SaveContact

let mapWebGetRoute =
    function
    | AllContacts -> "contacts"
    | NewContact -> "contacts/new"
    | EditContact i -> sprintf "contacts/edit?id=%d" i
    | AllGroups -> "groups"
    | Error e -> "error?e=" + urlencode e

let makeEditContactUrl i = EditContact i |> mapWebGetRoute |> String.prepend "/"

let mapWebPostRoute =
    function
    | DeleteContact -> "contacts/delete"
    | SaveContact -> "contacts/save"

let saveContactUrl = mapWebPostRoute SaveContact |> String.prepend "/"

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

let getPathR x = mapWebGetRoute x |> String.split '?' |> Array.nth 0 |> getPath
let postPathR x = mapWebPostRoute x |> postPath
let redirectR x = mapWebGetRoute x |> String.prepend "/" |> redirect

let e = XhtmlElement()
let s = e.Shortcut
let f = e.Formlets

let makeTable entities (proj: (string * (_ -> Node list)) list) = 
    let text t = &t
    let header = proj |> Seq.map (fst >> text >> List.singleton >> e.Th) |> e.Tr
    let makeRow g = proj |> Seq.map (snd >> (|>) g >> e.Td) |> e.Tr
    let rows = entities |> Seq.map makeRow |> Seq.toList
    e.Table (header::rows)

let layout title (body: Node list) = 
    [e.Html [
        e.Head [
            e.Title [ &title ]
        ]
        e.Body body
    ]]

let inline submit text = 
    e.Input ["type","submit"; "value", text]

let inline ihidden (name, value) = 
    e.Input ["type","hidden"; "name",name; "value",value]

let inline postForm url content = 
    e.Form ["method","post"; "action",url] content

let inline simplePostForm url text = 
    postForm url [ submit text ]

let postFormlet text url formlet =
    postForm url [
        yield! !++ formlet
        yield submit text
    ]

let link text url = e.A ["href", url] [ &text ]

let groupsView (groups: Group seq) = 
    layout "Manage contact groups"
        [makeTable groups ["Group name", fun c -> [ &c.Name ]]]

let idVersionFormlet (idVersion: int64 * int64) = pickler idVersion
let emptyIdVersionFormlet = idVersionFormlet (0L,0L)

let contactsView (contacts: Contact seq) error = 
    layout "Manage contacts"
        [
            makeTable contacts [
                "Name", fun c -> [ &c.Name ]
                "Email", fun c -> [ &c.Email ]
                "Phone", fun c -> [ &c.Phone ]
                "", fun c -> [ postFormlet "Delete" (mapWebPostRoute DeleteContact) (idVersionFormlet (c.Id, c.Version)) ]
                "", fun c -> [ link "Edit" (makeEditContactUrl c.Id) ]
            ]
            e.P [ &error ]
        ]

let showAllGroups cmgr = 
    Group.FindAll() cmgr |> Tx.get |> groupsView

let manageGroups cmgr ctx =
    wbview (showAllGroups cmgr) ctx

let manageGroupsAction : RouteConstraint * FAction =
    getPathR AllGroups, manageGroups connMgr

let showAllContacts cmgr = 
    Contact.FindAll() cmgr |> Tx.get |> contactsView

let manageContacts cmgr ctx = 
    let viewAllContacts = showAllContacts cmgr >> wbview
    viewAllContacts =<< getFlash <| ctx // need to delay explicitly

let manageContactsAction : RouteConstraint * FAction =
    getPathR AllContacts, manageContacts connMgr

let deleteContact cmgr =
    runPost emptyIdVersionFormlet
    >>= function
        | Formlet.Success (id,version) -> 
            match Contact.DeleteCascade id version cmgr with
            | Tx.Commit (Some _) -> redirectR AllContacts
            | Tx.Commit None -> 
                setFlash "Contact deleted or modified" >>. redirectR AllContacts
            | Tx.Rollback a -> redirectR (Error (a.ToString()))
            | Tx.Failed e -> redirectR (Error (e.ToString()))
        | Formlet.Failure (_,errors) -> redirectR (Error (sprintf "%A" errors))

let deleteContactAction: RouteConstraint * FAction =
    postPathR DeleteContact, deleteContact connMgr

let contactFormlet (c: Contact) =
    let idVersion = idVersionFormlet (c.Id, c.Version)
    let nameInput = f.Text(c.Name, required = true) |> f.WithLabel "Name"
    let phoneOrEmail = 
        let phoneInput = f.Tel(c.Phone) |> f.WithLabel "Phone:"
        let emailInput = f.Email(c.Email) |> f.WithLabel "Email:"
        let phoneOrEmail = yields t2 <*> phoneInput <*> emailInput
        let nonEmpty = String.IsNullOrWhiteSpace >> not
        let oneNonEmpty (a,b) = nonEmpty a || nonEmpty b
        phoneOrEmail |> satisfies (err oneNonEmpty (fun _ -> "Enter either a phone or an email"))

    yields (fun (i,v) n (p,e) -> { Contact.Id = i; Version = v; Name = n; Phone = p; Email = e })
    <*> idVersion
    <*> nameInput
    <*> phoneOrEmail

let emptyContactFormlet = contactFormlet Contact.Dummy

let contactWriteView title err (n: XNode list)=
    layout title
        [
            s.FormPost saveContactUrl [
                yield!!+ n
                yield e.P [ submit "Save" ]
                yield e.P [ &err ]
            ]
        ]

let contactEditView = contactWriteView "Edit contact"
let contactEditOkView = contactEditView ""

// monadic with named operators
let editContact cmgr =
    getQueryString "id"
    |> Result.bind 
        (function
         | None -> redirectR (Error "Missing contact id")
         | Some contactId -> 
            Int32.tryParse contactId
            |> Option.bind (fun i ->
                                match Contact.GetById i cmgr with
                                | Tx.Commit c -> c
                                | _ -> None)
            |> Option.map (fun c -> 
                                let editFormlet = contactFormlet c |> renderToXml
                                let view = contactEditOkView editFormlet
                                wbview view)
            |> Option.getOrElse (redirectR (Error "")))

type MaybeBuilder() =
    member x.Bind(m,f) = Option.bind f m
    member x.Return a = Some a

let maybe = MaybeBuilder()

// monadic using computation expressions
let editContact4 cmgr =
    result {
        let! contactId = getQueryString "id"
        let contact = 
            maybe {
                let! contactId = contactId // not very intuitive
                let! contactId = Int32.tryParse contactId
                let! contact = 
                    match Contact.GetById contactId cmgr with
                    | Tx.Commit c -> c
                    | _ -> None
                return contact
            }
        do! match contact with 
            | None -> redirectR (Error "Contact not found")
            | Some c -> 
                let editFormlet = contactFormlet c |> renderToXml
                let view = contactEditOkView editFormlet
                wbview view
    }

// alternative, only using option monad
let editContact2 cmgr (ctx: ControllerContext) =
    let contactId = ctx.HttpContext.Request.QueryString.["id"]
    let action = 
        Int32.tryParse contactId
        |> Option.bind (fun i -> Contact.GetById i cmgr |> Tx.get) // doesn't handle tx fail
        |> Option.map (fun c -> wbview (contactEditOkView (contactFormlet c |> renderToXml)))
        |> Option.getOrElse (redirectR (Error "Invalid contact id"))
    action ctx

// alternative, no-monad, only pattern matching
let editContact3 cmgr ctx =
    match getQueryString "id" ctx with
    | None -> redirectR (Error "Missing contact id") ctx
    | Some contactId ->
        match Int32.tryParse contactId with
        | None -> redirectR (Error "Invalid contact id") ctx
        | Some contactId -> 
            match Contact.GetById contactId cmgr with
            | Tx.Commit contact ->
                match contact with
                | None -> redirectR (Error "Contact not found") ctx
                | Some contact -> 
                    let editFormlet = contactFormlet contact |> renderToXml
                    let view = contactEditOkView editFormlet
                    wbview view ctx
            | _ -> redirectR (Error "DB Error") ctx

let editContactAction: RouteConstraint * FAction = 
    getPathR (EditContact 0L), editContact connMgr

let saveContact cmgr = 
    result {
        let! contactResult = runPost emptyContactFormlet
        do! match contactResult with
            | populatedForm, _, Some contact -> 
                match Contact.Upsert contact cmgr with
                | Tx.Commit (Some _) -> redirectR AllContacts
                | Tx.Commit None -> wbview (contactEditView "Contact deleted or modified, please go back and reload" populatedForm)
                | _ -> redirectR (Error "DB Error")
            | errorForm, _, None -> wbview (contactEditOkView errorForm)
    }

// alternative using monadic operators
let saveContact2 cmgr = 
    runPost emptyContactFormlet
    >>= function
        | populatedForm, _, Some contact -> 
            match Contact.Upsert contact cmgr with
            | Tx.Commit (Some _) -> redirectR AllContacts
            | Tx.Commit None -> wbview (contactEditView "Contact deleted or modified, please go back and reload" populatedForm)
            | _ -> redirectR (Error "DB Error")
        | errorForm, _, None -> wbview (contactEditOkView errorForm)

let saveContactAction: RouteConstraint * FAction = 
    postPathR SaveContact, saveContact connMgr

let contactNewView = contactWriteView "New contact" ""

let newContactAction: RouteConstraint * FAction = 
    getPathR NewContact, emptyContactFormlet |> renderToXml |> contactNewView |> wbview