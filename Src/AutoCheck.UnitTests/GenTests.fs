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
    test <@ actual
            |> Seq.filter (fun item -> item < lower || item > upper)
            |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Sized passes the current size`` seed (size : int) =
    let g = Gen.sized (fun s -> Gen.choose (-s, s))

    let actual = Gen.generate size seed g

    let upper =  size |> Math.Abs
    let lower = -size
    test <@ actual >= lower && actual <= upper @>

[<Theory; AutoData>]
let ``Elements generates one of the given values`` (xs : int []) (seeds : Generator<int>) size =
    let g = Gen.elements xs
    let seed i = seeds |> Seq.item i

    let actual =
        [ for i in 1..30 -> Gen.generate size (seed i) g ]

    test <@ xs
            |> Seq.except actual
            |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Resize overrides the size parameter`` (newSize : int) size seed =
    newSize <>! size
    let g = Gen.sized Gen.init |> Gen.resize newSize

    let actual = g |> Gen.generate size seed

    newSize =! actual

[<Theory; AutoData>]
let ``Oneof randomly uses one of the given generators`` size (seeds : Generator<int>) =
    let seed i = seeds |> Seq.item i
    let g1 = Gen.init 1
    let g2 = Gen.init 2
    let g3 = Gen.init 3

    let actual =
        [ for i in 1..9 ->
            Gen.oneof [ g1; g2; g3 ] |> Gen.generate size (seed i) ]

    let unexpected =
        seq {
            yield [ 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
            yield [ 2; 2; 2; 2; 2; 2; 2; 2; 2 ]
            yield [ 3; 3; 3; 3; 3; 3; 3; 3; 3 ]
        }
    test <@ unexpected
            |> Seq.forall (fun u -> u.Length = actual.Length && u <> actual) @>

[<Theory; AutoData>]
let ``Frequency chooses one of the given generators`` size (seeds : Generator<int>) =
    let seed i = seeds |> Seq.item i
    let g1 = Gen.init "a"
    let g2 = Gen.init "b"
    let g3 = Gen.init "c"

    let actual =
        let gn = Gen.frequency [ (1, g1); (2, g2); (3, g3) ]
        [ for i in 1..100 -> Gen.generate size (seed i) gn ]
        |> Seq.countBy id
        |> Seq.sortBy id
        |> Seq.toList

    let g1s = actual |> List.item 0 |> snd
    let g2s = actual |> List.item 1 |> snd
    let g3s = actual |> List.item 2 |> snd
    test <@ g1s < g2s && g2s < g3s @>

[<Theory; AutoData>]
let ``Variant modifies a generator using an integer seed`` size seed =
    let original = Gen.sized (fun s -> Gen.choose (-s, s))
    let modified = original |> Gen.variant (int DateTime.UtcNow.Ticks)

    let actual = modified |> Gen.generate size seed

    let unexpected = original |> Gen.generate size seed
    unexpected <>! actual

[<Theory; AutoData>]
let ``Two takes a Gen and returns a Gen of tuple`` (expected : int) size seed =
    let g = Gen.two (Gen.init expected)
    let actual = g |> Gen.generate size seed
    (expected, expected) =! actual

[<Theory; AutoData>]
let ``Three takes a Gen and returns a Gen of triple`` (expected : int) size seed =
    let g = Gen.three (Gen.init expected)
    let actual = g |> Gen.generate size seed
    (expected, expected, expected) =! actual

[<Theory; AutoData>]
let ``Four takes a Gen and returns a Gen of quaple`` (expected : int) size seed =
    let g = Gen.four (Gen.init expected)
    let actual = g |> Gen.generate size seed
    (expected, expected, expected, expected) =! actual
