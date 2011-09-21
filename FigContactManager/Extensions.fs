namespace FigContactManager

[<AutoOpen>]
module Result =

    open WingBeats
    open WingBeats.Xml
    open Figment
    open System.Web.Mvc

    let internal flashKey0 = "FigmentFlash0"
    let internal flashKey1 = "FigmentFlash1"

    let wbview (n: Node list) : FAction =
        fun ctx -> Renderer.Render(n, ctx.HttpContext.Response.Output)

    let setFlash (value: string) : FAction = 
        fun ctx -> ctx.Session.Set flashKey1 value

    let getFlash : ControllerContext -> string =
        fun ctx -> ctx.Session.Get flashKey0

module Filters = 
    open Figment

    let flash (a: FAction): FAction =
        fun ctx ->
            ctx.Session.Pop flashKey1 |> ctx.Session.Set flashKey0
            try
                a ctx
            finally
                ctx.Session.Remove flashKey0

[<AutoOpen>]
module FormletsExtensions =

    open System.Web.Mvc
    open Formlets
    open Figment.Extensions

    let runPost formlet (ctx: ControllerContext) =
        let env = EnvDict.fromFormAndFiles ctx.Request
        run formlet env

    let runGet formlet (ctx: ControllerContext) =
        let env = EnvDict.fromNV ctx.QueryString
        run formlet env

    let runParams formlet (ctx: ControllerContext) =
        let env = EnvDict.fromNV ctx.Request.Params
        run formlet env

type String =
    static member prepend prefix (s: string) =
        prefix + s
    static member split (sep: char) (s: string) =
        s.Split [|sep|]
