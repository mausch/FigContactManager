[<AutoOpen>]
module FigContactManager.Tests.Prelude

open Fuchu

type Assert =
    static member inline AreEqual(expected, actual) =
        if expected <> actual
            then failtestf "Expected: %A\nActual: %A" expected actual
    
    static member inline Contains(actual: string, expected) =
        if not (actual.Contains expected)
            then failtestf "Expected to contain: %s\nActual: %s" expected actual
        