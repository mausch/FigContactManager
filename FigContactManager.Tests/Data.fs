module FigContactManager.Data.Tests

open System
open System.Collections.Generic
open System.Data
open MbUnit.Framework
open Microsoft.FSharp.Reflection
open FigContactManager.Data
open FigContactManager.Model

let connectionString = "Data Source=:memory:;Version=3;New=True"
let createConnection() = createConnection connectionString

[<Test>]
let ``generate insert`` () =
    let sql,p = generateInsert (Contact.New "John" "555-1234" "john@example.com")
    let p = Seq.toList p
    printfn "%s" sql
    Assert.AreEqual("insert into Contact (Id,Version,Name,Phone,Email) values (null, @Version,@Name,@Phone,@Email); select last_insert_rowid();", sql)
    Assert.AreEqual(4, p.Length)
    Assert.AreEqual("@Version", p.[0].ParameterName)
    Assert.AreEqual("@Name", p.[1].ParameterName)
    Assert.AreEqual("@Phone", p.[2].ParameterName)
    Assert.AreEqual("@Email", p.[3].ParameterName)
    Assert.AreEqual(0L, unbox p.[0].Value)
    Assert.AreEqual("John", string p.[1].Value)
    Assert.AreEqual("555-1234", string p.[2].Value)
    Assert.AreEqual("john@example.com", string p.[3].Value)

[<Test>]
let ``generate delete`` () =
    let sql,p = generateDelete (Contact.NewWithId 2L "" "" "")
    let p = Seq.toList p
    printfn "%s" sql
    Assert.AreEqual("delete from Contact where id = @i; select changes();", sql)
    Assert.AreEqual(1, p.Length)
    Assert.AreEqual("@i", p.[0].ParameterName)
    Assert.AreEqual(2L, unbox p.[0].Value)

[<Test>]
let ``generate update`` () =
    let sql,p = generateUpdate (Contact.NewWithId 2L "nn" "pp" "ee")
    let p = p |> Seq.map (fun p -> p.ParameterName, p.Value) |> dict
    printfn "%s" sql
    Assert.AreEqual("update Contact set Version=@Version,Name=@Name,Phone=@Phone,Email=@Email where id = @id; select changes();", sql)
    Assert.AreEqual(5, p.Count)
    Assert.AreEqual(2L, unbox p.["@id"])
    Assert.AreEqual("nn", unbox p.["@Name"])
    Assert.AreEqual("pp", unbox p.["@Phone"])
    Assert.AreEqual("ee", unbox p.["@Email"])

[<Test>]
let ``generate versioned update``() =
    let sql,p = generateVersionedUpdate (Contact.New "name" "phone" "mail")
    printfn "%s" sql
    Assert.AreEqual("update Contact set Version=@Version,Name=@Name,Phone=@Phone,Email=@Email where id = @id and version = @oldversion; select changes();", sql)
    printfn "%A" p
    let p = p |> List.map (fun x -> x.ParameterName,x.Value) |> dict
    Assert.AreEqual("name", unbox p.["@Name"])
    Assert.AreEqual("phone", unbox p.["@Phone"])
    Assert.AreEqual("mail", unbox p.["@Email"])
    Assert.AreEqual(0L, unbox p.["@oldversion"])
    Assert.AreEqual(1L, unbox p.["@Version"])

[<Test>]
let ``generate versioned delete``() =
    let sql,p = generateVersionedDeleteId typeof<Contact> 1 2
    Assert.AreEqual("delete from Contact where id = @id and version = @version; select changes();", sql)
    let p = p |> List.map (fun x -> x.ParameterName,x.Value) |> dict
    Assert.AreEqual(1, unbox p.["@id"])
    Assert.AreEqual(2, unbox p.["@version"])

[<Test>]
let ``generate findall`` () =    
    let sql = generateFindAll typeof<Group> (Some (20,50))
    printfn "%s" sql
    Assert.AreEqual("select * from \"Group\" limit 20 offset 50", sql)

let tx = Tx.TransactionBuilder()

[<Test>]
let ``create contact``() =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema mgr [typeof<Contact>]
    let insert =
        tx {
            let! i = Contact.Insert (Contact.New "John" "555-1234" "john@example.com")
            let! j = Contact.Insert (Contact.New "George" "555-4447" "george@example.com")
            printfn "%A" i
            printfn "%A" j
            return i,j
        }
    insert mgr 
    |> Tx.get
    |> (fun (c1,c2) -> 
            Assert.AreEqual(1L, c1.Id)
            Assert.AreEqual(2L, c2.Id))

[<Test>]
let ``create group`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema mgr [typeof<Group>]
    let newGroup = Group.Insert { Id = 0L; Name = "Business" } |> Tx.map ignore
    newGroup mgr |> Tx.get |> ignore

[<Test>]
let ``delete group cascade`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema mgr [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
    let transaction = 
        tx {
            let! john = Contact.Insert (Contact.New "John" "555-1234" "john@example.com")
            let! business = Group.Insert { Id = 0L; Name = "Business" }
            let! john_business = ContactGroup.Insert { Id = 0L; Group = business.Id; Contact = john.Id }
            let! _ = Group.DeleteCascade business.Id
            let! count = Tx.execScalar "select count(*) from ContactGroup" []
            Assert.AreEqual(0L, Option.get count)
        }
    transaction mgr |> Tx.get |> ignore

[<Test>]
let ``find all groups`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema mgr [typeof<Group>]
    let transaction =
        tx {
            let! business = Group.Insert { Id = 0L; Name = "Business" }
            let! groups = Group.FindAll() |> Tx.map Seq.toList
            Assert.AreEqual(1, groups.Length)
            Assert.AreEqual(business, groups.[0])
            return ()
        }
    transaction mgr |> Tx.get |> ignore