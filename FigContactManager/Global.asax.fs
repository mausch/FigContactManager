namespace FigContactManager

open System
open System.Web
open System.Web.Mvc
open FigContactManager.Data

type MvcApplication() =
    inherit HttpApplication()

    let tx = Tx.TransactionBuilder()
    let InitializeDatabase() =
        let cs = System.Configuration.ConfigurationManager.ConnectionStrings.["sqlite"].ConnectionString
        use conn = createConnection cs
        createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
        let t =
            tx {
                let! john = Contact.Insert { Contact.Id = 0L; Name = ""; Phone = ""; Email = "" }
                return 0
            }
        t (Sql.withConnection conn) |> Tx.getOrFail ignore
        ()

    member this.Application_Start() = 
        InitializeDatabase()
        ()