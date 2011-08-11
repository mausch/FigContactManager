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
let ``generate insert`` () =
    let sql,p = 
        generateInsert { Id = 0L; Name = "John"; Phone = "555-1234"; Email = "john@example.com" }
        |> (fun (a,b) -> (a,Seq.toList b))
    printfn "%s" sql
    Assert.AreEqual("insert into Contact values (null, @Name,@Phone,@Email)", sql)
    Assert.AreEqual(3, p.Length)
    Assert.AreEqual("@Name", p.[0].ParameterName)
    Assert.AreEqual("@Phone", p.[1].ParameterName)
    Assert.AreEqual("@Email", p.[2].ParameterName)
    Assert.AreEqual("John", string p.[0].Value)
    Assert.AreEqual("555-1234", string p.[1].Value)
    Assert.AreEqual("john@example.com", string p.[2].Value)

    ()


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
