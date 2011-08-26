namespace FigContactManager

open System
open System.Collections.Generic
open System.Data
open Microsoft.FSharp.Reflection
open FigContactManager.Data

module Model =
    type Contact = {
        Id: int64
        Name: string
        Phone: string
        Email: string
    }
    with 
        static member NewWithId id name phone email = 
            { Id = id; Name = name; Phone = phone; Email = email }
        static member New = Contact.NewWithId 0L
        static member Dummy = Contact.New "" "" ""

    type Group = {
        Id: int64
        Name: string
    }
    with static member New name =
            { Id = 0L; Name = name }

    type ContactGroup = {
        Id: int64
        Group: int64
        Contact: int64
    }
    with static member New group contact =
            { Id = 0L; Group = group; Contact = contact }


    type ContactGroup with
        static member Insert (c: ContactGroup) =
            genericInsert c
            |> Tx.map (fun newId -> { c with Id = newId })
        static member Delete (c: ContactGroup) = genericDelete c
        static member DeleteByGroup (c: Group) =
            Tx.execNonQueryi "delete from ContactGroup where \"group\" = @g" [P("@g",c.Id)]
        static member DeleteByContactId (cid: int) =
            Tx.execNonQueryi "delete from ContactGroup where \"contact\" = @g" [P("@g",cid)]

    type Contact with
        static member Insert (c: Contact) =
            genericInsert c
            |> Tx.map (fun newId -> { c with Id = newId })
        static member private Delete (c: Contact) = genericDelete c
        static member Update (c: Contact) = genericUpdate c
        static member private DeleteById i =
            generateDeleteId typeof<Contact> i
            ||> Tx.execNonQueryi
        static member DeleteCascade (c: int) =
            ContactGroup.DeleteByContactId c >>. Contact.DeleteById c
        static member GetById i =
            generateGetById typeof<Contact> i
            ||> Tx.execReader
            |> Tx.map (Sql.mapFirst (Sql.asRecord<Contact> ""))
        static member FindAll(?limitOffset) = genericFindAll<Contact> limitOffset

    type Group with
        static member Insert (c: Group) =
            genericInsert c
            |> Tx.map (fun newId -> { c with Id = newId })
        static member private Delete (c: Group) = genericDelete c
        static member DeleteCascade (c: Group) =
            ContactGroup.DeleteByGroup c >>. Group.Delete c
        static member FindAll(?limitOffset) = genericFindAll<Group> limitOffset

    let connectionString = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString
    let connMgr = Sql.withNewConnection (fun () -> createConnection connectionString)
    