module LightCheck.Tests.ShrinkTests

open Xunit
open Xunit.Extensions
open Ploeh.AutoFixture
open Ploeh.AutoFixture.Xunit
open Swensen.Unquote

open LightCheck

[<Fact>]
let ``False shrinks to an empty list`` () =
    Shrink.bool false =! Seq.empty

[<Fact>]
let ``True shrinks to false`` () =
    Shrink.bool true |> Seq.exactlyOne =! false

[<Theory; AutoData>]
let ``Numbers are shrinked towards smaller ones`` (fixture : IFixture) =
    fixture.Customizations.Add(
        RandomNumericSequenceGenerator(-999L, 999L))
    let input = fixture.Create<int>()

    let actual = Shrink.number input

    let expected =
        input
        |> Seq.unfold (fun s -> Some(input - s, s / 2))
        |> Seq.tail
        |> Seq.append [ 0 ]
        |> Seq.takeWhile (fun el -> abs input > abs el)
        |> Seq.append (if input < 0 then Seq.singleton -input
                       else Seq.empty)
        |> Seq.distinct
    Seq.compareWith Operators.compare expected actual =! 0

[<Fact>]
let ``Injecting a function into a shrinker and back returns correct result`` () =
    let origin = Shrink.number
    let remote = origin |> Shrink.init |> Shrink.evaluate
    (-7 |> origin |> Seq.toList) =!
    (-7 |> remote |> Seq.toList)
