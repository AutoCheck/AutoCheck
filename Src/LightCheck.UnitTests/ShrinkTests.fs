module LightCheck.Tests.ShrinkTests

open Xunit
open Swensen.Unquote

open LightCheck

[<Fact>]
let ``False shrinks to an empty list`` () =
    Shrink.bool false =! []

[<Fact>]
let ``True shrinks to false`` () =
    Shrink.bool true =! [ false ]