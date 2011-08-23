module FigContactManager.Web

open System
open System.Web
open System.Web.Routing
open System.Web.Mvc
open Formlets
open Figment
open WingBeats
open WingBeats.Xml
open WingBeats.Xhtml
open FigContactManager.Data

module Option = 
    let getOrElse v =
        function
        | Some x -> x
        | _ -> v

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

let e = XhtmlElement()
let s = e.Shortcut

let makeTable entities (proj: (string * (_ -> Node list)) list) = 
    let text t = &t
    let header = proj |> Seq.map (fst >> text >> List.singleton >> e.Th) |> e.Tr
    let makeRow g = proj |> Seq.map (snd >> (|>) g >> e.Td) |> e.Tr
    let rows = entities |> Seq.map makeRow |> Seq.toList
    e.Table (header::rows)

let layout title (body: Node) = 
    [e.Html [
        e.Head [
            e.Title [ &title ]
        ]
        e.Body [ body ]
    ]]

let inline submit text = 
    e.Input ["type","submit"; "value", text]

let inline hidden (name, value) = 
    e.Input ["type","hidden"; "name",name; "value",value]

let inline postForm url content = 
    e.Form ["method","post"; "action",url] content

let inline simplePostForm url text = 
    postForm url [ submit text ]

let postFormValues text url values = 
    postForm url [
        for nv in values do
            yield hidden nv
        yield submit text
    ]

let groupsView (groups: Group seq) = 
    layout "Manage contact groups"
        (makeTable groups ["Group name", fun c -> [ &c.Name ]])

let contactsView (contacts: Contact seq) = 
    layout "Manage contacts"
        (makeTable contacts [
            "Name", fun c -> [ &c.Name ]
            "Email", fun c -> [ &c.Email ]
            "Phone", fun c -> [ &c.Phone ]
            "", fun c -> [ postFormValues "Delete" "/contacts/delete" ["id",c.Id.ToString()] ]
        ])

let showAllGroups cmgr = 
    Group.FindAll() cmgr |> Tx.get |> groupsView

let manageGroups cmgr ctx =
    wbview (showAllGroups cmgr) ctx

let manageGroupsAction : RouteConstraint * FAction =
    getPath "groups", manageGroups connMgr

let showAllContacts cmgr = 
    Contact.FindAll() cmgr |> Tx.get |> contactsView

let manageContacts cmgr ctx = 
    wbview (showAllContacts cmgr) ctx

let manageContactsAction : RouteConstraint * FAction =
    getPath "contacts", manageContacts connMgr

let deleteContact cmgr (ctx: ControllerContext) =
    let contactId = ctx.HttpContext.Request.Params.["id"]
    let action = 
        Int32.tryParse contactId
        |> Option.map (fun i -> 
                            Contact.DeleteById i cmgr |> Tx.get |> ignore
                            redirect "/contacts")
        |> Option.getOrElse (redirect "/error")
    action ctx

let deleteContactAction: RouteConstraint * FAction =
    postPath "contacts/delete", deleteContact connMgr