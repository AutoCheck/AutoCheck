module LightCheck.Tests.ShrinkTests

open Xunit
open Swensen.Unquote

open LightCheck

[<Fact>]
let ``False shrinks to an empty list`` () =
    let shrink = Shrink.evaluate Shrink.bool
    shrink false =! Seq.empty

[<Fact>]
let ``True shrinks to false`` () =
    let shrink = Shrink.evaluate Shrink.bool
    shrink true |> Seq.exactlyOne =! false