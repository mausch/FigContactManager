module FigContactManager.Web.Routes

open System
open FSharpx
open FigContactManager
open Figment

type WebGetRoute =
    | AllContacts
    | NewContact
    | EditContact of int64
    | AllGroups
    | NewGroup
    | EditGroup of int64
    | Error of string

type WebPostRoute =
    | DeleteContact
    | DeleteGroup
    | SaveContact
    | SaveGroup

type CRUDRoutes = {
    New: WebGetRoute
    Edit: int64 -> WebGetRoute
    All: WebGetRoute
    Save: WebPostRoute
    Delete: WebPostRoute
}

let groupRoutes = {
    New = NewGroup
    Edit = EditGroup
    All = AllGroups
    Save = SaveGroup
    Delete = DeleteGroup
}

let contactRoutes = {
    New = NewContact
    Edit = EditContact
    All = AllContacts
    Save = SaveContact
    Delete = DeleteContact
}

let mapWebGetRoute =
    function
    | AllContacts -> "/contacts"
    | NewContact -> "/contacts/new"
    | EditContact i -> sprintf "/contacts/edit?id=%d" i
    | AllGroups -> "/groups"
    | NewGroup -> "/groups/new"
    | EditGroup i -> sprintf "/groups/edit?id=%d" i
    | Error e -> "/error?e=" + urlencode e

let mapWebPostRoute =
    function
    | DeleteContact -> "/contacts/delete"
    | DeleteGroup -> "/groups/delete"
    | SaveContact -> "/contacts/save"
    | SaveGroup -> "/groups/save"

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

let baseUrl = String.split '?' >> Array.nth 0

let getPathR x = mapWebGetRoute x |> baseUrl |> getPath
let postPathR x = mapWebPostRoute x |> postPath
let redirectR x = mapWebGetRoute x |> redirect
