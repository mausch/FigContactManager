namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data
open FigContactManager.DataValidation
open FigContactManager.Validation

type MvcApplication() =
    inherit HttpApplication()

    let tx = Tx.TransactionBuilder()
    let InitializeDatabase() =
        let cs = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString
        use conn = createConnection cs
        createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        let t =
            tx {
                let! john = Contact.TryNew "John" "" "" |> getOrFail |> Contact.Insert
                return 0
            }
        let t = Tx.required t // run in a transaction
        t (Sql.withConnection conn) |> Tx.getOrFail ignore
        ()

    member this.Application_Start() = 
        InitializeDatabase()
        ()