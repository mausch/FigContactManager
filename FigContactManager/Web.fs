module FigContactManager.Web

open System.Web.Mvc
open Figment
open WingBeats
open WingBeats.Xhtml
open FigContactManager.Data

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet

let e = XhtmlElement()

let groupsView (groups: Group seq) = 
    [e.Html [
        e.Head [
            e.Title [ &"Manage contact groups" ]
        ]
        e.Body [
            e.Table [
                yield e.Tr [
                    e.Th [ &"Group name" ]
                ]
                for g in groups do
                    yield e.Tr [
                        e.Td [ &g.Name ]
                    ]
            ]
        ]
    ]]

let contactsView (contacts: Contact seq) = 
    [e.Html [
        e.Head [
            e.Title [ &"Manage contacts" ]
        ]
        e.Body [
            e.Table [
                yield e.Tr [
                    e.Th [ &"Name" ]
                    e.Th [ &"Email" ]
                    e.Th [ &"Phone" ]
                ]
                for c in contacts do
                    yield e.Tr [
                        e.Td [ &c.Name ]
                        e.Td [ &c.Email ]
                        e.Td [ &c.Phone ]
                    ]
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