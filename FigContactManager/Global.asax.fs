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

    static member InitializeDatabase connectionString =
        printfn "Initializing database..."
        use conn = createConnection connectionString
        createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        let connMgr = Sql.withConnection conn
        App.AddSampleData connMgr
        connMgr
        

    member this.Application_Start() = 
        App.InitializeDatabase connectionString |> ignore
        get "" (redirect "contacts")
        get "error" (contentf "<pre>%s</pre>" =<< (getQueryString "e" |> Reader.map Option.getOrDefault))
        let actions = [manageGroupsAction; manageContactsAction; deleteContactAction; editContactAction; saveContactAction; newContactAction]
        let actions = [for r,a in actions -> r, Filters.flash a]
        actions |> Seq.iter ((<||) action)
        ()

    member this.Application_End() =
        let reason = System.Web.Hosting.HostingEnvironment.ShutdownReason.ToString()
        printfn "Application_End. Reason: %s" reason