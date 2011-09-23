module FigContactManager.Web.Tests

open Formlets
open MbUnit.Framework
open FigContactManager
open FigContactManager.Data
open FigContactManager.Model
open FigContactManager.Data.Tests
open FigContactManager.Web
open WingBeats
open WingBeats.Xml

[<Test>]
let ``show all groups`` () =
    use conn = createConnection()
    let cmgr = Sql.withConnection conn
    App.InitializeDatabase cmgr
    let html = showAllGroups cmgr |> Renderer.RenderToString
    printfn "%s" html
    ()

[<Test>]
let ``contact formlet with empty phone and email gives error``() =
    let env = EnvDict.fromStrings [losSerializer.Serialize (1L,1L); "John"; ""; ""]
    printfn "%A" env
    match run emptyContactFormlet env with
    | Success c -> failwithf "formlet should not have succeeded: %A" c
    | Failure (_,errors) -> 
        Assert.AreEqual(1, errors.Length)
        Assert.AreEqual("Enter either a phone or an email", errors.[0])
    ()