module FigContactManager.Data.Tests

open System
open System.Collections.Generic
open System.Data
open MbUnit.Framework
open Microsoft.FSharp.Reflection
open FigContactManager.Data
open FigContactManager.Model

let createConnection() = createConnection connectionString

[<Test>]
let ``generate insert`` () =
    let sql,p = generateInsert (Contact.New "John" "555-1234" "john@example.com")
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
    let sql,p = generateDelete (Contact.NewWithId 2L "" "" "")
    let p = Seq.toList p
    printfn "%s" sql
    Assert.AreEqual("delete from Contact where id = @i", sql)
    Assert.AreEqual(1, p.Length)
    Assert.AreEqual("@i", p.[0].ParameterName)
    Assert.AreEqual(2L, unbox p.[0].Value)

[<Test>]
let ``generate update`` () =
    let sql,p = generateUpdate (Contact.NewWithId 2L "nn" "pp" "ee")
    let p = p |> Seq.map (fun p -> p.ParameterName, p.Value) |> dict
    printfn "%s" sql
    Assert.AreEqual("update Contact set Name=@Name,Phone=@Phone,Email=@Email where id = @id", sql)
    Assert.AreEqual(4, p.Count)
    Assert.AreEqual(2L, unbox p.["@id"])
    Assert.AreEqual("nn", unbox p.["@Name"])
    Assert.AreEqual("pp", unbox p.["@Phone"])
    Assert.AreEqual("ee", unbox p.["@Email"])

[<Test>]
let ``generate versioned update``() =
    let sql,p = generateVersionedUpdate (Contact.New "name" "phone" "mail")
    printfn "%s" sql
    Assert.AreEqual("update Contact set Version=@Version,Name=@Name,Phone=@Phone,Email=@Email where id = @id and version = @oldversion", sql)
    printfn "%A" p
    let p = p |> List.map (fun x -> x.ParameterName,x.Value) |> dict
    Assert.AreEqual("name", unbox p.["@Name"])
    Assert.AreEqual("phone", unbox p.["@Phone"])
    Assert.AreEqual("mail", unbox p.["@Email"])
    Assert.AreEqual(0L, unbox p.["@oldversion"])
    Assert.AreEqual(1L, unbox p.["@Version"])
    ()

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
    createSchema conn [typeof<Contact>]
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
    createSchema conn [typeof<Group>]
    let newGroup = Group.Insert { Id = 0L; Name = "Business" } |> Tx.map ignore
    newGroup mgr |> Tx.get |> ignore

[<Test>]
let ``delete group cascade`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
    let transaction = 
        tx {
            let! john = Contact.Insert (Contact.New "John" "555-1234" "john@example.com")
            let! business = Group.Insert { Id = 0L; Name = "Business" }
            let! john_business = ContactGroup.Insert { Id = 0L; Group = business.Id; Contact = john.Id }
            do! Group.DeleteCascade business
            let! count = Tx.execScalar "select count(*) from ContactGroup" []
            let count = Option.get count
            Assert.AreEqual(0L, count)
        }
    transaction mgr |> Tx.get |> ignore

[<Test>]
let ``find all groups`` () =
    use conn = createConnection()
    let mgr = Sql.withConnection conn
    createSchema conn [typeof<Group>]
    let transaction =
        tx {
            let! business = Group.Insert { Id = 0L; Name = "Business" }
            let! groups = Group.FindAll() |> Tx.map Seq.toList
            Assert.AreEqual(1, groups.Length)
            Assert.AreEqual(business, groups.[0])
            return ()
        }
    transaction mgr |> Tx.get |> ignore
    ()