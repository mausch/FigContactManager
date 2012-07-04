module FigContactManager.Tests.ModelValidation

open Fuchu
open FigContactManager.Data
open FigContactManager.Model
open FigContactManager.ModelValidation

[<Tests>]
let tests = 
    testList "model validation" [
        testCase "contact with invalid phone" <| fun _ ->
            let c = Contact.TryNew "John" "abcd" "ee@example.com"
            match c with
            | Choice1Of2 _ -> 
                failtest "should not have succeeded"
            | Choice2Of2 errors -> 
                Assert.AreEqual(1, errors.Length)
                //printfn "errors: %A" errors

        testCase "contact ok" <| fun _ ->
            let c = Contact.TryNew "John" "555-1234" "ee@example.com"
            match c with
            | Choice1Of2 _ -> ()
            | Choice2Of2 errors -> 
                //printfn "errors: %A" errors
                failtest "should not have failed"
    ]
