module MbUnit.TestDSL

open System.IO
open MbUnit.Framework

// DSL to define test suites and cases

let inline (=>>) name tests =
    let suite = TestSuite(name)
    Seq.iter suite.Children.Add tests
    suite :> Test

let inline (=>) name (test: unit -> unit) =
    TestCase(name, Gallio.Common.Action test) :> Test

let inline (+>) f =
     Seq.map (fun (name, partialTest) ->
                    name => f partialTest)

let inline (==>) name test = name,test        
