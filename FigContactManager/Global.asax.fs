namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data
open FigContactManager.DataValidation
open FigContactManager.Validation
open FigContactManager.Web
open Figment

type App() =
    inherit HttpApplication()

    static member AddSampleData conn =
        let tx = Tx.TransactionBuilder()
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
        t conn |> Tx.get |> ignore

    static member InitializeDatabase connectionString =
        printfn "Initializing database..."
        use conn = createConnection connectionString
        createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        let connMgr = Sql.withConnection conn
        App.AddSampleData connMgr
        connMgr
        

    member this.Application_Start() = 
        App.InitializeDatabase connectionString |> ignore
        get "" (content "Hi!")
        let actions = [manageGroupsAction; manageContactsAction; deleteContactAction; editContactAction; saveContactAction; newContactAction]
        actions |> Seq.iter ((<||) action)
        ()

    member this.Application_End() =
        let reason = System.Web.Hosting.HostingEnvironment.ShutdownReason.ToString()
        printfn "Application_End. Reason: %s" reason