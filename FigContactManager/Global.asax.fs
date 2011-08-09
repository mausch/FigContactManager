namespace FigContactManager

open System
open System.Web
open System.Web.Mvc

type MvcApplication() =
    inherit HttpApplication()
    member this.Application_Start() = 
        ()