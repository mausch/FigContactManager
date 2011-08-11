namespace FigContactManager

open System
open System.Collections.Generic
open System.Data
open Microsoft.FSharp.Reflection

module Data =
    type Contact = {
        Id: int64
        Name: string
        Phone: string
        Email: string
    }

    type Group = {
        Id: int64
        Name: string
    }

    type ContactGroup = {
        Group: int64
        Contact: int64
    }

    let internal strEq (a: string) (b: string) = 
        StringComparer.InvariantCultureIgnoreCase.Equals(a, b)

    let createSchema conn types =
        let exec a = Sql.execNonQuery (Sql.withConnection conn) a [] |> ignore
        let sqlType t =
            match t with
            | x when x = typeof<int> -> "int"
            | x when x = typeof<string> -> "varchar"
            | x when x = typeof<bool> -> "int"
            | x when x = typeof<DateTime> -> "datetime"
            | x -> failwithf "Don't know how to express type %A in database" x
        let keywords = HashSet<_>(["order"], StringComparer.InvariantCultureIgnoreCase)
        let escape s =
            if keywords.Contains s
                then sprintf "\"%s\"" s // sqlite-specific quote
                else s
        let createTable (escape: string -> string) (sqlType: Type -> string) (t: Type) =
            let fields = FSharpType.GetRecordFields t |> Seq.filter (fun p -> strEq p.Name "id" |> not)
            let table = escape t.Name
            let drop = sprintf "drop table if exists %s" table
            let fields = 
                let fieldType (t: Type) =
                    let nullable = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
                    let sqlType = 
                        let t = 
                            if nullable
                                then t.GetGenericArguments().[0]
                                else t
                        sqlType t
                    let nullable = if nullable then "" else "not"
                    sprintf "%s %s null" sqlType nullable
                fields |> Seq.map (fun f -> sprintf "%s %s" (escape f.Name) (fieldType f.PropertyType))
            let fields = String.Join(",", Seq.toArray fields)
            let create = sprintf "create table %s (id integer primary key autoincrement, %s)" table fields
            //printfn "%s" create
            exec drop
            exec create
            ()
        types |> Seq.iter (createTable escape sqlType)


    let internal P = Sql.Parameter.make

    let internal (>>=) f x = Tx.bind x f

    let internal selectLastId = "select last_insert_rowid();"

    let generateInsert a =
        let allfields = a.GetType() |> Sql.recordFields
        let names = allfields |> Seq.skip 1 |> Seq.map (fun f -> "@" + f) |> Seq.toList
        let sql = sprintf "insert into %s (%s) values (null, %s); %s" 
                    (a.GetType().Name)
                    (String.concat "," allfields)
                    (String.concat "," names) 
                    selectLastId
        let values = Sql.recordValues a |> Seq.skip 1
        let parameters = Seq.zip names values |> Sql.parameters
        sql,parameters

    let insertContact (c: Contact) =
        generateInsert c 
        ||> Tx.execScalar
        |> Tx.map Option.get
        |> Tx.map (fun newId -> { c with Id = newId })

