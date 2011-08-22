module FigContactManager.Web

open System.Web.Mvc
open Formlets
open Figment
open WingBeats
open WingBeats.Xhtml
open FigContactManager.Data

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet

let e = XhtmlElement()

let makeTable entities (proj: (string * (_ -> Xml.Node list)) list) = 
    let text t = &t
    let header = proj |> Seq.map (fst >> text >> List.singleton >> e.Th) |> e.Tr
    let makeRow g = proj |> Seq.map (snd >> (|>) g >> e.Td) |> e.Tr
    let rows = entities |> Seq.map makeRow |> Seq.toList
    e.Table (header::rows)

let groupsView (groups: Group seq) = 
    [e.Html [
        e.Head [
            e.Title [ &"Manage contact groups" ]
        ]
        e.Body [
            makeTable groups ["Group name", fun c -> [ &c.Name ]]
        ]
    ]]

let contactsView (contacts: Contact seq) = 
    [e.Html [
        e.Head [
            e.Title [ &"Manage contacts" ]
        ]
        e.Body [
            makeTable contacts [
                "Name", fun c -> [ &c.Name ]
                "Email", fun c -> [ &c.Email ]
                "Phone", fun c -> [ &c.Phone ]
            ]
        ]
    ]]

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