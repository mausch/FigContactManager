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
open FigContactManager.Data
open WingBeats.Formlets
open System.Xml.Linq

module Option = 
    let getOrElse v =
        function
        | Some x -> x
        | _ -> v

type String =
    static member prepend prefix s =
        prefix + s
    static member split (sep: char) (s: string) =
        s.Split [|sep|]

module Array =
    let nth i a = 
        Array.get a i

type WebGetRoute =
    | AllContacts
    | EditContact of int64
    | AllGroups
    | Error

type WebPostRoute =
    | DeleteContact of int64
    | SaveContact

let mapWebGetRoute =
    function
    | AllContacts -> "contacts"
    | EditContact i -> sprintf "contacts/edit?id=%d" i
    | AllGroups -> "groups"
    | Error -> "error"

let makeEditContactUrl i = EditContact i |> mapWebGetRoute |> String.prepend "/"

let mapWebPostRoute =
    function
    | DeleteContact i -> "contacts/delete", ["id", i.ToString()]
    | SaveContact -> "contacts/save", []

let saveContactUrl = mapWebPostRoute SaveContact |> fst |> String.prepend "/"

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

let getPathR x = mapWebGetRoute x |> String.split '?' |> Array.nth 0 |> getPath
let postPathR x = mapWebPostRoute x |> fst |> postPath
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

let postFormValues text (url, values) = 
    postForm url [
        for nv in values do
            yield ihidden nv
        yield submit text
    ]

let link text url = e.A ["href", url] [ &text ]

let groupsView (groups: Group seq) = 
    layout "Manage contact groups"
        [makeTable groups ["Group name", fun c -> [ &c.Name ]]]

let contactsView (contacts: Contact seq) = 
    layout "Manage contacts"
        [makeTable contacts [
            "Name", fun c -> [ &c.Name ]
            "Email", fun c -> [ &c.Email ]
            "Phone", fun c -> [ &c.Phone ]
            "", fun c -> [ postFormValues "Delete" (mapWebPostRoute (DeleteContact c.Id)) ]
            "", fun c -> [ link "Edit" (makeEditContactUrl c.Id) ]
        ]]

let contactFormlet (c: Contact) =
    let idHidden = pickler c.Id
    let nameInput = f.Text(c.Name, required = true) |> f.WithLabel "Name"
    let phoneInput = f.Tel(c.Phone) |> f.WithLabel "Phone:"
    let emailInput = f.Email(c.Email) |> f.WithLabel "Email:"
    let phoneOrEmail = yields t2 <*> phoneInput <*> emailInput
    let nonEmpty = String.IsNullOrWhiteSpace >> not
    let oneNonEmpty (a,b) = nonEmpty a || nonEmpty b
    let phoneOrEmail = phoneOrEmail |> satisfies (err oneNonEmpty (fun _ -> "Enter either a phone or an email"))
    yields (fun i n (p,e) -> Contact.NewWithId i n p e)
    <*> idHidden
    <*> nameInput
    <*> phoneOrEmail

let contactEdit (n: XNode list)=
    layout "Edit contact" 
        [
            s.FormPost saveContactUrl [
                yield!!+ n
                yield submit "Save"
            ]
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
    wbview (showAllContacts cmgr) ctx

let manageContactsAction : RouteConstraint * FAction =
    getPathR AllContacts, manageContacts connMgr

let deleteContact cmgr (ctx: ControllerContext) =
    let contactId = ctx.HttpContext.Request.Params.["id"]
    let action = 
        Int32.tryParse contactId
        |> Option.map (fun i -> 
                            Contact.DeleteCascade i cmgr |> Tx.get |> ignore
                            redirectR AllContacts)
        |> Option.getOrElse (redirectR Error)
    action ctx

let deleteContactAction: RouteConstraint * FAction =
    postPathR (DeleteContact 0L), deleteContact connMgr

let editContact cmgr (ctx: ControllerContext) =
    let contactId = ctx.HttpContext.Request.QueryString.["id"]
    let action = 
        Int32.tryParse contactId
        |> Option.bind (fun i -> Contact.GetById i cmgr |> Tx.get)
        |> Option.map (fun c -> wbview (contactEdit (contactFormlet c |> renderToXml)))
        |> Option.getOrElse (redirectR Error)
    action ctx

let editContactAction: RouteConstraint * FAction = 
    getPathR (EditContact 0L), editContact connMgr

let saveContact cmgr (ctx: ControllerContext) =
    let contactResult = runPost (contactFormlet Contact.Dummy) ctx
    let action = 
        match contactResult with
        | Success contact -> 
            match Contact.Update contact cmgr with
            | Tx.Commit _ -> redirectR AllContacts
            | _ -> redirectR Error
        | Failure (errorForm, _) -> wbview (contactEdit errorForm)
    action ctx

let saveContactAction: RouteConstraint * FAction = 
    postPathR SaveContact, saveContact connMgr