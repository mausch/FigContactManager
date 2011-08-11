module FigContactManager.Data.Tests

open System
open System.Collections.Generic
open System.Data
open MbUnit.Framework
open Microsoft.FSharp.Reflection
open FigContactManager.Data

let createConnection() =
    let cs = System.Configuration.ConfigurationManager.ConnectionStrings.["Sqlite_InMemory"].ConnectionString
    createConnection cs

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
    

let tx = Tx.TransactionBuilder()

[<Test>]
let ``create contact``() =
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
    insert mgr 
    |> Tx.getOrFail (fun (c1,c2) -> 
                        Assert.AreEqual(1L, c1.Id)
                        Assert.AreEqual(2L, c2.Id))

[<Test>]
let ``create group`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Group>]
    let newGroup = insertGroup { Id = 0L; Name = "Business" } |> Tx.map ignore
    newGroup mgr |> Tx.getOrFail ignore

[<Test>]
let ``delete group cascade`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
    let transaction = 
        tx {
            let! john = insertContact { Id = 0L; Name = "John"; Phone = "555-1234"; Email = "john@example.com" }
            let! business = insertGroup { Id = 0L; Name = "Business" }
            let! john_business = insertContactGroup { Id = 0L; Group = business.Id; Contact = john.Id }
            do! deleteGroupCascade business
            let! count = Tx.execScalar "select count(*) from ContactGroup" []
            let count = Option.get count
            Assert.AreEqual(0L, count)
        }
    transaction mgr |> Tx.getOrFail ignore

