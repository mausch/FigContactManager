module FigContactManager.Web

open System.Web.Mvc
open Figment
open WingBeats
open WingBeats.Xhtml
open FigContactManager.Data

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

let showAllGroups cmgr = 
    Group.FindAll() cmgr |> Tx.get |> groupsView

let manageGroups cmgr ctx =
    wbview (showAllGroups cmgr) ctx

let manageGroupsAction : RouteConstraint * FAction =
    (ifInsensitivePathIs "groups" &&. ifMethodIsGet), manageGroups connMgr