module AutoCheck.UnitTests.GenTests

open Xunit.Extensions
open Ploeh.AutoFixture.Xunit
open Ploeh.AutoFixture
open Swensen.Unquote
open AutoCheck
open System

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
    >! 1

[<Theory; AutoData>]
let ``Choose produces elements in range`` (seeds : Generator<int>) (size : int) count =
    let upper =  size |> Math.Abs
    let lower = -size
    let g = Gen.choose (lower, upper)
    let seed i = seeds |> Seq.item i

    let actual =
        [ for i in 1..count -> Gen.generate size (seed i) g ]

    test <@ actual |> Seq.exists (fun item -> item >= lower || item <= upper) @>
    test <@ actual |> Seq.filter (fun item -> item  < lower || item  > upper) |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Sized passes the current size`` seed (size : int) =
    let g = Gen.sized (fun s -> Gen.choose (-s, s))

    let actual = Gen.generate size seed g

    let upper =  size |> Math.Abs
    let lower = -size
    test <@ actual >= lower && actual <= upper @>

[<Theory; AutoData>]
let ``Elements generates one of the given values`` (xs : int[]) (seeds : Generator<int>) size =
    let g = Gen.elements xs
    let seed i = seeds |> Seq.item i

    let actual =
        [ for i in 1..30 -> Gen.generate size (seed i) g ]

    test <@ xs |> Seq.except actual |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Resize overrides the size parameter`` (newSize : int) size seed =
    newSize <>! size
    let g = Gen.sized Gen.init |> Gen.resize newSize
    let actual = g |> Gen.generate size seed

    newSize =! actual
