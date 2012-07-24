namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data
open FigContactManager.Model
open FigContactManager.ModelValidation
open FigContactManager.Web.Actions
open Figment
open FSharpx
open FSharpx.Reader

type App() =
    inherit HttpApplication()
    let connectionString = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString

    static member AddSampleData conn =
        let tx = Tx.TransactionBuilder()
        let choiceToTx =
            function
            | Choice1Of2 a -> fun _ -> Tx.Commit a
            | Choice2Of2 e -> fun _ -> Tx.Rollback e

        let applyToChoice f x = choiceToTx x |> Tx.bind f

        let t =
            tx {
                let ccccc = Contact.Insert
                let! john = Contact.TryNew "John" "555-1234" "john@example.com" 0L |> applyToChoice Contact.Insert
                let! jennifer = Contact.TryNew "Jennifer" "554-9988" "jennifer@example.com" 0L |> applyToChoice Contact.Insert
                let! friends = Group.TryNew "Friends" 0L |> applyToChoice Group.Insert
                let! work = Group.TryNew "Work" 0L |> applyToChoice Group.Insert
                do! ContactGroup.TryNew friends.Id john.Id |> applyToChoice ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew work.Id john.Id |> applyToChoice ContactGroup.Insert |> Tx.map ignore
                do! ContactGroup.TryNew friends.Id jennifer.Id |> applyToChoice ContactGroup.Insert |> Tx.map ignore
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

        App.InitializeDatabase connMgr

        let actions = [groupActions; contactActions] |> List.collect ((|>) connMgr)
        let actions = [for r,a in actions -> r, Filters.flash a]
        for r,a in actions do action r a
        ()

    member this.Application_End() =
        let reason = System.Web.Hosting.HostingEnvironment.ShutdownReason.ToString()
        printfn "Application_End. Reason: %s" reason