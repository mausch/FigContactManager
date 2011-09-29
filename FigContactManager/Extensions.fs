namespace FigContactManager

[<AutoOpen>]
module Result =

    open WingBeats.Xml
    open Figment

    let wbview (n: Node list) : FAction =
        fun ctx -> Renderer.Render(n, ctx.HttpContext.Response.Output)


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
