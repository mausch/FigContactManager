module FigContactManager.DataValidation.Tests

open MbUnit.Framework
open FigContactManager.Data
open FigContactManager.DataValidation

[<Test>]
let ``contact with invalid phone`` () =
    let c = Contact.TryNew "John" "abcd" "ee@example.com"
    match c with
    | Choice1Of2 _ -> 
        Assert.Fail "should not have succeeded"
    | Choice2Of2 errors -> 
        Assert.AreEqual(1, errors.Length)
        printfn "errors: %A" errors

[<Test>]
let ``contact ok`` () =
    let c = Contact.TryNew "John" "555-1234" "ee@example.com"
    match c with
    | Choice1Of2 _ -> ()
    | Choice2Of2 errors -> 
        printfn "errors: %A" errors
        Assert.Fail "should not have failed"
        
