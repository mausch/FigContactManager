module FigContactManager.Web.Routes

open System
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
    | AllContacts -> "/contacts"
    | NewContact -> "/contacts/new"
    | EditContact i -> sprintf "/contacts/edit?id=%d" i
    | EditGroup i -> sprintf "/groups/edit?id=%d" i
    | AllGroups -> "/groups"
    | Error e -> "/error?e=" + urlencode e

let mapWebPostRoute =
    function
    | DeleteContact -> "/contacts/delete"
    | SaveContact -> "/contacts/save"
    | SaveGroup -> "/groups/save"

let getPath p = ifInsensitivePathIs p &&. ifMethodIsGet
let postPath p = ifInsensitivePathIs p &&. ifMethodIsPost

let getPathR x = mapWebGetRoute x |> String.split '?' |> Array.nth 0 |> getPath
let postPathR x = mapWebPostRoute x |> postPath
let redirectR x = mapWebGetRoute x |> redirect
