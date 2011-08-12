module FigContactManager.DataValidation

open System
open System.Text.RegularExpressions
open FigContactManager.Data
open FigContactManager.Validation

let inline private (<*>) f x = apv f x
let inline private (<!>) f x = Choice1Of2 f <*> x
let inline private (>>=) f x = bind f x

let private nonEmpty msg x =
    if String.IsNullOrWhiteSpace x
        then Choice2Of2 [msg]
        else Choice1Of2 x

let private regex rx msg =
    let rx = Regex(rx, RegexOptions.Compiled)
    fun x ->
        if rx.IsMatch x
            then Choice1Of2 x
            else Choice2Of2 [msg]
            
let private phone =
    // from http://msdn.microsoft.com/en-us/library/ff650303.aspx
    regex @"^[01]?[- .]?(\([2-9]\d{2}\)|[2-9]\d{2})[- .]?\d{3}[- .]?\d{4}$"
        "Invalid phone number"

let private email = 
    // from http://msdn.microsoft.com/en-us/library/ff650303.aspx
    regex @"^(?("")("".+?""@)|(([0-9a-zA-Z]((\.(?!\.))|[-!#\$%&'\*\+/=\?\^`\{\}\|~\w])*)(?<=[0-9a-zA-Z])@))(?(\[)(\[(\d{1,3}\.){3}\d{1,3}\])|(([0-9a-zA-Z][-\w]*[0-9a-zA-Z]\.)+[a-zA-Z]{2,6}))$"
        "Invalid email address"

let private positiveId x =
    if x > 0L
        then Choice1Of2 x
        else Choice2Of2 ["Invalid id"]

type Contact with
    static member Validate (c: Contact) =
        Contact.New 
        <!> nonEmpty "Name can't be empty" c.Name 
        <*> phone c.Phone 
        <*> email c.Email
    static member TryNew name phone email =
        Contact.New name phone email |> Contact.Validate

type Group with
    static member Validate (g: Group) =
        Group.New
        <!> nonEmpty "Name can't be empty" g.Name
    static member TryNew name =
        Group.New name |> Group.Validate

type ContactGroup with
    static member Validate (g: ContactGroup) =
        ContactGroup.New
        <!> positiveId g.Group
        <*> positiveId g.Contact
    static member TryNew group contact =
        ContactGroup.New group contact |> ContactGroup.Validate