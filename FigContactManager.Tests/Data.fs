﻿module FigContactManager.Tests.Data

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
    let sql,p = generateInsert { Id = 0L; Name = "John"; Phone = "555-1234"; Email = "john@example.com" }
    let p = Seq.toList p
    printfn "%s" sql
    Assert.AreEqual("insert into Contact (Id,Name,Phone,Email) values (null, @Name,@Phone,@Email); select last_insert_rowid();", sql)
    Assert.AreEqual(3, p.Length)
    Assert.AreEqual("@Name", p.[0].ParameterName)
    Assert.AreEqual("@Phone", p.[1].ParameterName)
    Assert.AreEqual("@Email", p.[2].ParameterName)
    Assert.AreEqual("John", string p.[0].Value)
    Assert.AreEqual("555-1234", string p.[1].Value)
    Assert.AreEqual("john@example.com", string p.[2].Value)

[<Test>]
let ``generate delete`` () =
    let sql,p = generateDelete { Name = ""; Id = 2L; Phone = ""; Email = "" }
    let p = Seq.toList p
    printfn "%s" sql
    Assert.AreEqual("delete from Contact where id = @i", sql)
    Assert.AreEqual(1, p.Length)
    Assert.AreEqual("@i", p.[0].ParameterName)
    Assert.AreEqual(2L, unbox p.[0].Value)

[<Test>]
let ``generate update`` () =
    let sql,p = generateUpdate { Name = "nn"; Id = 2L; Phone = "pp"; Email = "ee" }
    let p = p |> Seq.map (fun p -> p.ParameterName, p.Value) |> dict
    printfn "%s" sql
    Assert.AreEqual("update Contact set Name=@Name,Phone=@Phone,Email=@Email where id = @id", sql)
    Assert.AreEqual(4, p.Count)
    Assert.AreEqual(2L, unbox p.["@id"])
    Assert.AreEqual("nn", unbox p.["@Name"])
    Assert.AreEqual("pp", unbox p.["@Phone"])
    Assert.AreEqual("ee", unbox p.["@Email"])
    

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

[<Test>]
let ``delete group cascade`` () =
    let tx = Tx.TransactionBuilder()
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
