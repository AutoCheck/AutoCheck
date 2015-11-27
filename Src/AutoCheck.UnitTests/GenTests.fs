module AutoCheck.UnitTests.GenTests

open Xunit.Extensions
open Ploeh.AutoFixture.Xunit
open Ploeh.AutoFixture
open Swensen.Unquote
open AutoCheck

[<Theory; AutoData>]
let ``Same seed produces same elements`` size count seed =
    let g = Gen.choose (-size, size)

    let actual =
        [ for i in 1..count -> Gen.generate size seed g ]

    actual
    |> Seq.distinct
    |> Seq.length
    =! 1

[<Theory; AutoData>]
let ``Different seed produces random elements`` size (seeds : Generator<int>) count =
    let g = Gen.choose (-size, size)
    let seed i = seeds |> Seq.item i

    let actual =
        [ for i in 1..count -> Gen.generate size (seed i) g ]

    actual
    |> Seq.distinct
    |> Seq.length
    >! count / 3
