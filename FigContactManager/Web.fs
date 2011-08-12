module FigContactManager.Web

open System.Web.Mvc
open Figment
open WingBeats
open WingBeats.Xhtml
open FigContactManager.Data

let e = XhtmlElement()

let connectionString = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString

let connMgr = Sql.withNewConnection (fun () -> createConnection connectionString)

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

let manageContactGroups ctx = 
    let html = Group.FindAll() connMgr |> Tx.getOrFail id |> groupsView 
    wbview html ctx

let manageContactGroupsAction : RouteConstraint * FAction =
    (ifPathIs "Groups" &&. ifMethodIsGet), manageContactGroups