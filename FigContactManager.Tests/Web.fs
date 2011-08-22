module FigContactManager.Web.Tests

open MbUnit.Framework
open FigContactManager
open FigContactManager.Data
open FigContactManager.Data.Tests
open FigContactManager.Web
open WingBeats
open WingBeats.Xml

[<Test>]
let ``show all groups`` () =
    let cmgr = App.InitializeDatabase connectionString
    let html = showAllGroups cmgr |> Renderer.RenderToString
    printfn "%s" html
    ()
