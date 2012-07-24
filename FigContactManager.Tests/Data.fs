module FigContactManager.Tests.Data

open Fuchu
open System
open System.Collections.Generic
open System.Data
open Microsoft.FSharp.Reflection
open FigContactManager.Data
open FigContactManager.Model

let connectionString = "Data Source=:memory:;Version=3;New=True"
let createConnection() = createConnection connectionString

let tx = Tx.TransactionBuilder()

let withConnection f () = 
    use conn = createConnection()
    f (Sql.withConnection conn)

[<Tests>]
let tests = 
    testList "Data" [
        testList "SQL generation" [
            testCase "insert" <| fun _ ->
                let sql,p = generateInsert (Contact.New "John" "555-1234" "john@example.com")
                let p = Seq.toList p
                //printfn "%s" sql
                Assert.Equal("insert SQL", "insert into Contact (Id,Version,Name,Phone,Email) values (null, @Version,@Name,@Phone,@Email); select last_insert_rowid();", sql)
                Assert.Equal("parameter count", 4, p.Length)

                Assert.Equal("1. parameter name", "@Version", p.[0].ParameterName)
                Assert.Equal("2. parameter name", "@Name", p.[1].ParameterName)
                Assert.Equal("3. parameter name", "@Phone", p.[2].ParameterName)
                Assert.Equal("4. parameter name", "@Email", p.[3].ParameterName)

                Assert.Equal("1. parameter value", 0L, unbox p.[0].Value)
                Assert.Equal("2. parameter value", "John", string p.[1].Value)
                Assert.Equal("3. parameter value", "555-1234", string p.[2].Value)
                Assert.Equal("4. parameter value", "john@example.com", string p.[3].Value)

            testCase "delete" <| fun _ ->
                let sql,p = generateDelete (Contact.NewWithId 2L "" "" "")
                let p = Seq.toList p
                //printfn "%s" sql
                Assert.Equal("delete SQL", "delete from Contact where id = @i; select changes();", sql)
                Assert.Equal("parameter count", 1, p.Length)
                Assert.Equal("parameter name", "@i", p.[0].ParameterName)
                Assert.Equal("parameter value", 2L, unbox p.[0].Value)

            testCase "update" <| fun _ ->
                let sql,p = generateUpdate (Contact.NewWithId 2L "nn" "pp" "ee")
                let p = p |> Seq.map (fun p -> p.ParameterName, p.Value) |> dict
                //printfn "%s" sql
                Assert.Equal("update SQL", "update Contact set Version=@Version,Name=@Name,Phone=@Phone,Email=@Email where id = @id; select changes();", sql)
                Assert.Equal("parameter count", 5, p.Count)
                Assert.Equal("id parameter value", 2L, unbox p.["@id"])
                Assert.Equal("name parameter value", "nn", unbox p.["@Name"])
                Assert.Equal("phone parameter value", "pp", unbox p.["@Phone"])
                Assert.Equal("email parameter value", "ee", unbox p.["@Email"])

            testCase "versioned update" <| fun _ ->
                let sql,p = generateVersionedUpdate (Contact.New "name" "phone" "mail")
                //printfn "%s" sql
                Assert.Equal("update SQL", "update Contact set Version=@Version,Name=@Name,Phone=@Phone,Email=@Email where id = @id and version = @oldversion; select changes();", sql)
                //printfn "%A" p
                let p = p |> List.map (fun x -> x.ParameterName,x.Value) |> dict
                Assert.Equal("name parameter value", "name", unbox p.["@Name"])
                Assert.Equal("phone parameter value", "phone", unbox p.["@Phone"])
                Assert.Equal("mail parameter value", "mail", unbox p.["@Email"])
                Assert.Equal("old version paramter value", 0L, unbox p.["@oldversion"])
                Assert.Equal("version parameter value", 1L, unbox p.["@Version"])

            testCase "versioned delete" <| fun _ ->
                let sql,p = generateVersionedDeleteId typeof<Contact> 1 2
                Assert.Equal("delete SQL", "delete from Contact where id = @id and version = @version; select changes();", sql)
                let p = p |> List.map (fun x -> x.ParameterName,x.Value) |> dict
                Assert.Equal("id parameter value", 1, unbox p.["@id"])
                Assert.Equal("version parameter value", 2, unbox p.["@version"])

            testCase "findall" <| fun _ ->
                let sql = generateFindAll typeof<Group> (Some (20,50))
                //printfn "%s" sql
                Assert.Equal("select SQL", "select * from \"Group\" limit 20 offset 50", sql)
        ]

        testList "connection" [
            yield! testFixture withConnection [
                "create contact", fun mgr ->
                    createSchema mgr [typeof<Contact>]
                    let insert =
                        tx {
                            let! i = Contact.Insert (Contact.New "John" "555-1234" "john@example.com" 0L)
                            let! j = Contact.Insert (Contact.New "George" "555-4447" "george@example.com" 0L)
                            //printfn "%A" i
                            //printfn "%A" j
                            return i,j
                        }
                    let c1,c2 = insert mgr |> Tx.get
                    Assert.Equal("c1 ID", 1L, c1.Id)
                    Assert.Equal("c2 ID", 2L, c2.Id)

                "create group", fun mgr ->
                    createSchema mgr [typeof<Group>]
                    let newGroup = Group.Insert { Id = 0L; Name = "Business"; User = 0L } |> Tx.map ignore
                    newGroup mgr |> Tx.get |> ignore

                "delete group cascade", fun mgr ->
                    createSchema mgr [typeof<Contact>; typeof<Group>; typeof<ContactGroup>]
                    let transaction = 
                        tx {
                            let! john = Contact.Insert (Contact.New "John" "555-1234" "john@example.com" 0L)
                            let! business = Group.Insert { Id = 0L; Name = "Business"; User = john.Id }
                            let! john_business = ContactGroup.Insert { Id = 0L; Group = business.Id; Contact = john.Id }
                            let! _ = Group.DeleteCascade business.Id
                            let! count = Tx.execScalar "select count(*) from ContactGroup" []
                            Assert.Equal("count result", Some 0L, count)
                        }
                    transaction mgr |> Tx.get |> ignore

                "find all groups", fun mgr ->
                    createSchema mgr [typeof<Group>]
                    let transaction =
                        tx {
                            let! business = Group.Insert { Id = 0L; Name = "Business"; User = 0L }
                            let! groups = Group.FindAll() |> Tx.map Seq.toList
                            Assert.Equal("group count", 1, groups.Length)
                            Assert.Equal("first group", business, groups.[0])
                            return ()
                        }
                    transaction mgr |> Tx.get |> ignore
            ]
        ]
    ]
