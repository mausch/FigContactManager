module FigContactManager.Web.Tests

open Fuchu
open FSharpx
open Formlets
open MbUnit.Framework
open FigContactManager
open FigContactManager.Data
open FigContactManager.Model
open FigContactManager.Data.Tests
open FigContactManager.Web.Actions
open FigContactManager.Web.Views
open WingBeats
open WingBeats.Xml
open Figment
open Figment.Testing
open System.Web
open System.Web.Mvc
open System.Web.Routing

[<Tests>]
let tests = 
    testList "Web" [
        testCase "contact formlet with empty phone and email gives error" <| fun _ ->
            let env = EnvDict.fromStrings [losSerializer.Serialize (1L,1L); "John"; ""; ""]
            printfn "%A" env
            match run contactViews.EmptyEditFormlet env with
            | Success c -> failwithf "formlet should not have succeeded: %A" c
            | Failure (_,errors) -> 
                Assert.AreEqual(1, errors.Length)
                Assert.AreEqual("Enter either a phone or an email", errors.[0])

        testCase "delete contact with inexisting contact" <| fun _ ->
            use conn = createConnection()
            let mgr = Sql.withConnection conn
            createSchema mgr [typeof<Contact>; typeof<ContactGroup>]
            let serialized = losSerializer.Serialize (1L,1L)
            let redirectTo = ref ""
            let cookie : HttpCookie ref = ref null
            let ctx = 
                buildRequest "GET" ""
                |> withForm (NameValueCollection.fromSeq ["f0",serialized])
                |> withFiles []
                |> withResponse
                    { new HttpResponseBase() with
                        override x.Redirect(url, _) = redirectTo := url 
                        override x.SetCookie c = cookie := c }
                |> buildCtx
            (snd contactActions'.Delete) mgr ctx
            Assert.AreEqual("/contacts", !redirectTo)
            Assert.Contains(base64decode (!cookie).Value, "deleted or modified")
    ]