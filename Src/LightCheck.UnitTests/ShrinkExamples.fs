module LightCheck.Tests.ShrinkExamples

open Xunit
open Xunit.Extensions
open Ploeh.AutoFixture
open Ploeh.AutoFixture.Xunit
open Swensen.Unquote

open LightCheck
open LightCheck.Shrink

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

[<Theory; AutoData>]
let ``Lists are shrinked based on the supplied shrinker`` (sq : int seq) =
    let l = Seq.toList sq
    let shrinker = Shrink number

    let actual = Shrink.list l shrinker

    let expected =
        let (Shrink shrf) = shrinker
        let rec shrink xs =
            match xs with
            | []       -> Seq.empty
            | (h :: t) ->
                seq {
                    yield []
                    for h' in    shrf h  -> h' :: t
                    for t' in (shrink t) -> h  :: t'
                }
        shrink l
    Seq.compareWith Operators.compare expected actual =! 0
