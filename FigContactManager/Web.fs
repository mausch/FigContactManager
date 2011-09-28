namespace FigContactManager.Web

open System

module Routes =

    open FSharpx
    open FigContactManager
    open Figment

    type WebGetRoute =
        | AllContacts
        | NewContact
        | EditContact of int64
        | EditGroup of int64
        | AllGroups
        | Error of string

    type WebPostRoute =
        | DeleteContact
        | SaveContact
        | SaveGroup

    let mapWebGetRoute =
        function
        | AllContacts -> "contacts"
        | NewContact -> "contacts/new"
        | EditContact i -> sprintf "contacts/edit?id=%d" i
        | EditGroup i -> sprintf "groups/edit?id=%d" i
        | AllGroups -> "groups"
        | Error e -> "error?e=" + urlencode e

    let makeEditContactUrl i = EditContact i |> mapWebGetRoute |> String.prepend "/"
    let makeEditGroupUrl i = EditGroup i |> mapWebGetRoute |> String.prepend "/"

    let mapWebPostRoute =
        function
        | DeleteContact -> "contacts/delete"
        | SaveContact -> "contacts/save"
        | SaveGroup -> "groups/save"

    let saveContactUrl = mapWebPostRoute SaveContact |> String.prepend "/"
    let saveGroupUrl = mapWebPostRoute SaveGroup |> String.prepend "/"

    let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
    let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

    let getPathR x = mapWebGetRoute x |> String.split '?' |> Array.nth 0 |> getPath
    let postPathR x = mapWebPostRoute x |> postPath
    let redirectR x = mapWebGetRoute x |> String.prepend "/" |> redirect

module Views = 

    open System.Xml.Linq
    open FSharpx
    open Routes
    open Formlets
    open WingBeats
    open WingBeats.Xhtml
    open WingBeats.Xml
    open WingBeats.Formlets
    open FigContactManager.Model

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

    let groupsView (groups: Group seq) error = 
        layout "Manage contact groups"
            [
                makeTable groups [
                    "Group name", fun c -> [ &c.Name ]
                    //"", fun c -> [ postFormlet "Delete" (mapWebPostRoute DeleteContact) (idVersionFormlet (c.Id, c.Version)) ]
                    "", fun c -> [ link "Edit" (makeEditGroupUrl c.Id) ]
                ]
                e.P [ &error ]
            ]

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

    let contactFormlet (c: Contact) =
        let idVersion = idVersionFormlet (c.Id, c.Version)
        let nameInput = f.Text(c.Name, required = true) |> f.WithLabel "Name"
        let phoneOrEmail = 
            let phoneInput = f.Tel(c.Phone) |> f.WithLabel "Phone:"
            let emailInput = f.Email(c.Email) |> f.WithLabel "Email:"
            let phoneOrEmail = yields tuple2 <*> phoneInput <*> emailInput
            let nonEmpty = String.IsNullOrWhiteSpace >> not
            let oneNonEmpty (a,b) = nonEmpty a || nonEmpty b
            phoneOrEmail |> satisfies (err oneNonEmpty (fun _ -> "Enter either a phone or an email"))

        yields (fun (i,v) n (p,e) -> { Contact.Id = i; Version = v; Name = n; Phone = p; Email = e })
        <*> idVersion
        <*> nameInput
        <*> phoneOrEmail

    let groupFormlet (c: Group) =
        let nameInput = f.Text(c.Name, required = true) |> f.WithLabel "Name"

        yields (fun i n -> { Group.Id = i; Name = n })
        <*> pickler c.Id
        <*> nameInput

    let saveView url title err (n: XNode list) =
        layout title
            [
                s.FormPost url [
                    yield!!+ n
                    yield e.P [ submit "Save" ]
                    yield e.P [ &err ]
                ]
            ]

    let emptyGroupFormlet = groupFormlet Group.Dummy
    let emptyContactFormlet = contactFormlet Contact.Dummy

    let contactWriteView = saveView saveContactUrl
    let contactEditView = contactWriteView "Edit contact"
    let contactEditOkView = contactEditView ""

    let groupWriteView = saveView saveGroupUrl
    let groupEditView = groupWriteView "Edit group"
    let groupEditOkView = groupEditView ""

module Actions =
    open System.Xml.Linq
    open System.Web.Mvc
    open FSharpx
    open FSharpx.Reader
    open Figment
    open FigContactManager
    open FigContactManager.Model
    open Views
    open Routes
    open Formlets

    let manage findAll view =
        let view = findAll() >> Tx.get >> view
        fun cmgr ->
            getFlash >>= fun err -> view cmgr err |> wbview

    let manageGroups = manage Group.FindAll groupsView

    type RouteAndAction = RouteConstraint * (Sql.ConnectionManager -> FAction)

    let manageGroupsAction : RouteAndAction =
        getPathR AllGroups, manageGroups

    let manageContacts = manage Contact.FindAll contactsView

    let manageContactsAction : RouteAndAction = 
        getPathR AllContacts, manageContacts

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

    let deleteContactAction : RouteAndAction =
        postPathR DeleteContact, deleteContact

    let getIdFromQueryString : ControllerContext -> int option =     
        getQueryString "id" |> Reader.map (Option.bind Int32.parse)

    let edit name getById editFormlet view cmgr = 
        getIdFromQueryString
        |> Reader.map (Option.bind (fun i ->
                                    match getById i cmgr with
                                    | Tx.Commit c -> c
                                    | _ -> None))
        |> Reader.map (Option.map (fun c -> 
                                    let editForm = editFormlet c |> renderToXml
                                    let view = view editForm
                                    wbview view))
        |> Reader.bind (Option.getOrElse (redirectR (Error (sprintf "%s not found" name))))


    let editGroup x = edit "Group" Group.GetById groupFormlet groupEditOkView x
    let editContact x = edit "Contact" Contact.GetById contactFormlet contactEditOkView x

    let editAction route action =
        getPathR (route 0L), fun c -> noCache >>. action c

    let editContactAction : RouteAndAction = editAction EditContact editContact
    let editGroupAction : RouteAndAction = editAction EditGroup editGroup

    let save name formlet upsert allRoute editView editOkView cmgr = 
        runPost formlet
        >>= function
            | populatedForm, _, Some entity -> 
                match upsert entity cmgr with
                | Tx.Commit (Some _) -> redirectR allRoute
                | Tx.Commit None -> 
                    let msg = sprintf "%s deleted or modified, please go back and reload" name
                    wbview (editView msg populatedForm)
                | _ -> redirectR (Error "DB Error")
            | errorForm, _, None -> wbview (editOkView errorForm)

    let saveContact = save "Contact" emptyContactFormlet Contact.Upsert AllContacts contactEditView contactEditOkView

    let saveGroup = save "Group" emptyGroupFormlet Group.Upsert AllGroups groupEditView groupEditOkView

    let saveContactAction : RouteAndAction =
        postPathR SaveContact, saveContact

    let saveGroupAction : RouteAndAction = 
        postPathR SaveGroup, saveGroup

    let contactNewView = contactWriteView "New contact" ""

    let newContact : FAction = emptyContactFormlet |> renderToXml |> contactNewView |> wbview

    let newContactAction : RouteConstraint * FAction =
        getPathR NewContact, newContact