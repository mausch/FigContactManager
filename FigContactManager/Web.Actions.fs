module FigContactManager.Web.Actions

open System
open System.Xml.Linq
open System.Web.Mvc
open FSharpx
open FSharpx.Reader
open Figment
open FigContactManager
open FigContactManager.Model
open FigContactManager.Web.Views
open FigContactManager.Web.Routes
open Formlets

type RouteAndAction = RouteConstraint * (Sql.ConnectionManager -> FAction)

get "/" (redirectR AllContacts)
let errorUrl = Error "" |> mapWebGetRoute |> baseUrl
get errorUrl (fun ctx -> contentf "<pre>%s</pre>" ctx.QueryString.["e"] ctx)


let manage findAll view =
    let view = findAll() >> Tx.get >> view
    fun cmgr ->
        getFlash >>= fun err -> view cmgr err |> wbview

let manageGroups = manage Group.FindAll groupsView
let manageContacts = manage Contact.FindAll contactsView

let manageGroupsAction : RouteAndAction =
    getPathR AllGroups, manageGroups

let manageContactsAction : RouteAndAction = 
    getPathR AllContacts, manageContacts

let error a = 
    redirectR (Error (sprintf "%A" a))

let delete name allRoute deleteEntity formlet cmgr = 
    runPost formlet
    >>= function
        | Formlet.Success x -> 
            match deleteEntity x cmgr with
            | Tx.Commit (Some _) -> redirectR allRoute
            | Tx.Commit None -> 
                sprintf "%s previously deleted or modified" name |> setFlash >>. redirectR allRoute
            | Tx.Rollback a -> error a
            | Tx.Failed e -> error e
        | Formlet.Failure (_, errors) -> error errors

let deleteContact = delete "Contact" AllContacts Contact.DeleteCascade emptyIdVersionFormlet

let deleteContactAction : RouteAndAction =
    postPathR DeleteContact, deleteContact

let deleteGroup = delete "Group" AllGroups Group.DeleteCascade (pickler 0L)

let deleteGroupAction: RouteAndAction = 
    postPathR DeleteGroup, deleteGroup

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
    |> Reader.bind (Option.getOrElse (error (sprintf "%s not found" name)))


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
            | _ -> error "DB Error"
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