module FigContactManager.Tests.Data

open System
open System.Collections.Generic
open System.Data
open MbUnit.Framework
open Microsoft.FSharp.Reflection
open FigContactManager.Data

let createConnection() =
    let conn = new System.Data.SQLite.SQLiteConnection("Data Source=:memory:;Version=3;New=True")
    conn.Open()
    conn :> IDbConnection

[<Test>]
let ``create contact``() =
    let tx = Tx.TransactionBuilder()
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Contact>]
    let insert =
        tx {
            let! i = insertContact { Id = 0L; Name = "John"; Phone = "555-1234"; Email = "john@example.com" }
            let! j = insertContact { Id = 0L; Name = "George"; Phone = "555-4447"; Email = "george@example.com" }
            printfn "%A" i
            printfn "%A" j
            return i,j
        }
    match insert mgr with
    | Tx.Commit (c1,c2) ->
        Assert.AreEqual(1L, c1.Id)
        Assert.AreEqual(2L, c2.Id)
    | Tx.Failed e -> raise e
    | Tx.Rollback _ -> failwith "rollback"
    ()
