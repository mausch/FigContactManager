module FigContactManager.ModelValidation

open System
open System.Text.RegularExpressions
open FigContactManager.Model
open FSharpx.Choice
open FSharpx.Validation

let nonEmpty msg x =
    if String.IsNullOrWhiteSpace x
        then Choice2Of2 [msg]
        else Choice1Of2 x

let private regex rx =
    let rx = Regex(rx, RegexOptions.Compiled)
    fun msg x ->
        if rx.IsMatch x
            then Choice1Of2 x
            else Choice2Of2 [msg]
            
let phone =
    // from http://stackoverflow.com/questions/123559/a-comprehensive-regex-for-phone-number-validation/123666#123666
    regex @"^(?:(?:\+?1\s*(?:[.-]\s*)?)?(?:\(\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\s*\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\s*(?:[.-]\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\s*(?:[.-]\s*)?([0-9]{4})(?:\s*(?:#|x\.?|ext\.?|extension)\s*(\d+))?$"
        "Invalid phone number"

let email = 
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