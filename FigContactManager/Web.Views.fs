module FigContactManager.Web.Views

open System
open System.Xml.Linq
open FSharpx
open FigContactManager.Web.Routes
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

