namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data
open FigContactManager.Model
open FigContactManager.ModelValidation
open FigContactManager.Web
open Figment
open FSharpx
open FSharpx.Reader

type App() =
    inherit HttpApplication()
    let connectionString = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString

    static member AddSampleData conn =
        let tx = Tx.TransactionBuilder()
        let t =
            tx {
                let! john = Contact.TryNew "John" "555-1234" "john@example.com" |> Choice.get |> Contact.Insert
                let! jennifer = Contact.TryNew "Jennifer" "554-9988" "jennifer@example.com" |> Choice.get |> Contact.Insert
                let! friends = Group.TryNew "Friends" |> Choice.get |> Group.Insert
                let! work = Group.TryNew "Work" |> Choice.get |> Group.Insert
                do! ContactGroup.TryNew friends.Id john.Id |> Choice.get |> ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew work.Id john.Id |> Choice.get |> ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew friends.Id jennifer.Id |> Choice.get |> ContactGroup.Insert |> Tx.map ignore
                return ()
            }
        let t = Tx.required t // run in a transaction
        t conn |> Tx.get |> ignore

    static member InitializeDatabase (connMgr: Sql.ConnectionManager) =
        printfn "Initializing database..."
        createSchema connMgr [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        App.AddSampleData connMgr       

    member this.Application_Start() = 
        let connMgr = Sql.withNewConnection (fun () -> Data.createConnection connectionString)
        get "" (redirect "contacts")
        get "error" (contentf "<pre>%s</pre>" =<< (getQueryString "e" |> Reader.map Option.getOrDefault))

        App.InitializeDatabase connMgr

        let dbActions = [manageGroupsAction; manageContactsAction; deleteContactAction; editContactAction; saveContactAction]
        let stdActions = [newContactAction]
        let actions = [for r,a in dbActions -> r, a connMgr] @ stdActions
        let actions = [for r,a in actions -> r, Filters.flash a]
        for r,a in actions do action r a
        ()

    member this.Application_End() =
        let reason = System.Web.Hosting.HostingEnvironment.ShutdownReason.ToString()
        printfn "Application_End. Reason: %s" reason