namespace FigContactManager

open System
open System.Collections.Generic
open System.Data
open Microsoft.FSharp.Reflection

module Data =
    let createConnection (connectionString: string) =
        let conn = new System.Data.SQLite.SQLiteConnection(connectionString)
        conn.Open()
        conn :> IDbConnection

    let private strEq (a: string) (b: string) = 
        StringComparer.InvariantCultureIgnoreCase.Equals(a, b)

    let private keywords = 
        let k = ["order"; "group"]
        HashSet<_>(k, StringComparer.InvariantCultureIgnoreCase)
    let private escape s =
        if keywords.Contains s
            then sprintf "\"%s\"" s // sqlite-specific quote
            else s

    let createSchema conn types =
        let exec a = Sql.execNonQuery conn a [] |> ignore
        let sqlType t =
            match t with
            | x when x = typeof<int> -> "int"
            | x when x = typeof<int64> -> "int"
            | x when x = typeof<bool> -> "int"
            | x when x = typeof<string> -> "varchar"
            | x when x = typeof<DateTime> -> "datetime"
            | x -> failwithf "Don't know how to express type %A in database" x
        let createTable (escape: string -> string) (sqlType: Type -> string) (t: Type) =
            let fields = 
                FSharpType.GetRecordFields t 
                |> Seq.filter (fun p -> strEq p.Name "id" |> not) // convention: PK is named "id"
            let table = escape t.Name // convention: type name = table name
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
                // convention: record field name = table column name
            let fields = String.Join(",", Seq.toArray fields)
            let create = sprintf "create table %s (id integer primary key autoincrement, %s)" table fields
            //printfn "%s" create
            exec drop
            exec create
            ()
        types |> Seq.iter (createTable escape sqlType)


    let P = Sql.Parameter.make

    let private selectLastId = "select last_insert_rowid();"
    let private selectChanges = "select changes();"

    let generateInsert a =
        let allfields = a.GetType() |> Sql.recordFields
        // convention: first field is ID
        let names = allfields |> Seq.skip 1 |> Seq.map (sprintf "@%s") |> Seq.toList
        let sql = sprintf "insert into %s (%s) values (null, %s); %s" 
                    (escape <| a.GetType().Name) // convention: type name = table name
                    (allfields |> Seq.map escape |> String.concat ",")
                    (String.concat "," names) 
                    selectLastId
        let values = Sql.recordValues a |> Seq.skip 1
        let parameters = Seq.zip names values |> Sql.parameters
        sql,parameters

    let generateDeleteId (t: Type) value = 
        let name = "@i"
        let sql = sprintf "delete from %s where id = %s" // convention: PK is called 'id'
                    (escape t.Name) // convention: type name = table name
                    name
        sql,[P(name,value)]

    let generateVersionedDeleteId (t: Type) pk version = 
        let idParam = "@id"
        let versionParam = "@version"
        // convention: PK is called 'id'
        // convention: Version field is called 'version'
        let sql = sprintf "delete from %s where id = %s and version = %s; %s"
                    (escape t.Name) // convention: type name = table name
                    idParam
                    versionParam
                    selectChanges
        sql,[P(idParam, pk); P(versionParam, version)]

    let genericVersionedDeleteId (t: Type) pk version = 
        generateVersionedDeleteId t pk version
        ||> Tx.execScalar
        |> Tx.map (function
                    | None | Some 0L -> None
                    | Some _ -> Some ())

    let generateDelete a =
        // convention: first field is ID
        let value = a |> Sql.recordValues |> Seq.head
        generateDeleteId (a.GetType()) value

    let genericDelete c =
        generateDelete c ||> Tx.execNonQueryi

    let generateUpdate a =
        // convention: first field is ID
        let idValue = a |> Sql.recordValues |> Seq.head
        let idField = "@id"
        let allFieldsButId = a.GetType() |> Sql.recordFields |> Seq.skip 1 
        let fieldsAndParams = 
            allFieldsButId
            |> Seq.map (fun f -> sprintf "%s=@%s" (escape f) f)
            |> Seq.toList
            |> String.concat ","
        let sql = sprintf "update %s set %s where id = %s" // convention: PK is called 'id'
                    (escape <| a.GetType().Name) // convention: type name = table name
                    fieldsAndParams 
                    idField
        let values = a |> Sql.recordValues |> Seq.skip 1
        let names = allFieldsButId |> Seq.map (sprintf "@%s")
        let parameters = Seq.zip names values |> Sql.parameters |> Seq.toList
        let parameters = P(idField, idValue)::parameters
        sql,parameters

    let generateVersionedUpdate a =
        let idField = "@id"
        let versionField = "@oldversion"
        let recordValues = a |> Sql.recordValues |> Seq.toList
        let idValue = recordValues |> Seq.head // convention: first field is ID
        let oldVersion = recordValues |> Seq.nth 1 |> unbox // convention: second field is version
        let newVersion = oldVersion + 1L // convention: version field is int64
        let recordFields = a.GetType() |> Sql.recordFields
        let allFieldsButId = recordFields |> Seq.skip 1 |> Seq.toList
        let allFieldsButIdAndVersion = allFieldsButId |> Seq.skip 1 |> Seq.toList
        let fieldsAndParams = 
            allFieldsButId
            |> Seq.map (fun f -> sprintf "%s=@%s" (escape f) f)
            |> Seq.toList
            |> String.concat ","
        // convention: PK is called 'id'
        // convention: concurrency version field is called 'version'
        let sql = sprintf "update %s set %s where id = %s and version = %s; %s"
                    (escape <| a.GetType().Name) // convention: type name = table name
                    fieldsAndParams 
                    idField
                    versionField
                    selectChanges
        let values = recordValues |> Seq.skip 2 |> Seq.toList
        let values = (box newVersion)::values
        let names = allFieldsButId |> Seq.map (sprintf "@%s")
        let parameters = Seq.zip names values |> Sql.parameters |> Seq.toList
        let parameters = P(idField, idValue)::parameters
        let parameters = P(versionField, oldVersion)::parameters
        sql,parameters

    let genericUpdate c = 
        generateUpdate c ||> Tx.execNonQueryi

    let genericVersionedUpdate (incrVersion: 'a -> 'a) c = 
        generateVersionedUpdate c 
        ||> Tx.execScalar
        |> Tx.map (function
                    | None | Some 0L -> None
                    | Some _ -> Some (incrVersion c))

    let genericInsert c =
        generateInsert c 
        ||> Tx.execScalar
        |> Tx.map Option.get

    let generateFindAll (t: Type)  =
        let sql = sprintf "select * from %s" (escape t.Name) // convention: type name = table name
        function
        | Some (l,o) -> sprintf "%s limit %d offset %d" sql l o
        | _ -> sql

    let genericFindAll<'a> limitOffset =
        let sql = generateFindAll typeof<'a> limitOffset
        Tx.execReader sql [] |> Tx.map (Sql.map (Sql.asRecord<'a> ""))

    let generateGetById (t: Type) =
        let name = "@i"
        let sql = sprintf "select * from %s where id = %s" // convention: PK is named "id"
                    (escape t.Name) name // convention: type name = table name
        fun (id: int) -> sql, [P(name, id)] // convention: PK is int
