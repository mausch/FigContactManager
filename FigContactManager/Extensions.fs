namespace FigContactManager

open WingBeats
open WingBeats.Xml
open Figment

[<AutoOpen>]
module Result =
    let wbview (n: Node list) : Helpers.FAction =
        fun ctx -> Renderer.Render(n, ctx.HttpContext.Response.Output)
