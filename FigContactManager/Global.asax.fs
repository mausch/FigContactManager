namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data
open FigContactManager.DataValidation
open FigContactManager.Validation
open Figment

type MvcApplication() =
    inherit HttpApplication()

    let tx = Tx.TransactionBuilder()
    let InitializeDatabase() =
        let cs = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString
        use conn = createConnection cs
        createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        let t =
            tx {
                let! john = Contact.TryNew "John" "555-1234" "john@example.com" |> getOrFail |> Contact.Insert
                let! jennifer = Contact.TryNew "Jennifer" "554-9988" "jennifer@example.com" |> getOrFail |> Contact.Insert
                let! friends = Group.TryNew "Friends" |> getOrFail |> Group.Insert
                let! work = Group.TryNew "Work" |> getOrFail |> Group.Insert
                do! ContactGroup.TryNew friends.Id john.Id |> getOrFail |> ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew work.Id john.Id |> getOrFail |> ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew friends.Id jennifer.Id |> getOrFail |> ContactGroup.Insert |> Tx.map ignore
                return ()
            }
        let t = Tx.required t // run in a transaction
        t (Sql.withConnection conn) |> Tx.getOrFail ignore
        ()

    member this.Application_Start() = 
        InitializeDatabase()
        get "" (content "Hi!")
        ()