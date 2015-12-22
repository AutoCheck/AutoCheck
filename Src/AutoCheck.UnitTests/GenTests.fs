module AutoCheck.UnitTests.GenTests

open System
open AutoCheck
open AutoCheck.Gen
open Ploeh.AutoFixture.Xunit
open Ploeh.AutoFixture
open Swensen.Unquote
open Xunit.Extensions

[<Theory; AutoData>]
let ``Same seed produces same elements`` size count seed =
    let g = Gen.choose (-size, size)

    let actual =
        [ for i in 1..count -> Gen.generate seed g ]

    actual
    |> Seq.distinct
    |> Seq.length
    =! 1

[<Theory; AutoData>]
let ``Different seed produces random elements`` size (seeds : Generator<int>) count =
    let g = Gen.choose (-size, size) |> Gen.resize size
    let seed i = seeds |> Seq.item i

    let actual =
        [ for i in 1..count -> Gen.generate (seed i) g ]

    actual
    |> Seq.distinct
    |> Seq.length
    >! 1

[<Theory; AutoData>]
let ``Choose produces elements in range`` (size : int) =
    let upper =  size |> Math.Abs
    let lower = -size

    let actual =
        Gen.choose (lower, upper)
        |> Gen.resize size
        |> Gen.sample

    test <@ actual
            |> Seq.exists (fun item -> item >= lower || item <= upper) @>
    test <@ actual
            |> Seq.filter (fun item -> item  < lower || item  > upper)
            |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Sized passes the current size`` seed (size : int) =
    let g = Gen.sized (fun s -> Gen.choose (-s, s))

    let actual = Gen.generate seed g

    let upper =  size |> Math.Abs
    let lower = -size
    test <@ actual >= lower && actual <= upper @>

[<Theory; AutoData>]
let ``Elements generates one of the given values`` (xs : int []) =
    let actual =
        xs
        |> Gen.elements
        |> Gen.sample

    test <@ actual
            |> Seq.distinct
            |> Seq.except xs
            |> Seq.isEmpty @>

[<Theory; AutoData>]
let ``Resize overrides the size parameter`` (newSize : int) size seed =
    newSize <>! size
    let g = Gen.sized Gen.init |> Gen.resize newSize

    let actual = g |> Gen.generate seed

    newSize =! actual

[<Theory; AutoData>]
let ``OneOf randomly uses one of the given generators`` () =
    let g1 = Gen.init 1
    let g2 = Gen.init 2
    let g3 = Gen.init 3

    let actual = Gen.oneOf [ g1; g2; g3 ] |> Gen.sample

    let unexpected =
        seq {
            yield [ 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1 ]
            yield [ 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2 ]
            yield [ 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3 ]
        }
    test <@ unexpected
            |> Seq.forall (fun u -> u.Length = actual.Length && u <> actual) @>

[<Theory; AutoData>]
let ``Frequency chooses one of the given generators`` () =
    let g1 = Gen.init 1
    let g2 = Gen.init 2
    let g3 = Gen.init 3

    let actual =
        Gen.frequency [ (100, g1)
                        (200, g2)
                        (300, g3) ]
        |> Gen.sample
        |> Seq.countBy id
        |> Seq.sortBy id
        |> Seq.toList

    let g1s = actual |> List.item 0 |> snd
    let g2s = actual |> List.item 1 |> snd
    let g3s = actual |> List.item 2 |> snd
    test <@ g1s < g2s && g2s < g3s @>

[<Theory; AutoData>]
let ``Return in gen workflow returns correct result`` seed (s : string) =
    let run g = Gen.generate seed g
    let actual = gen { return s } |> run
    (Gen.init s |> run) =! actual

[<Theory; AutoData>]
let ``Variant modifies a generator using an integer seed`` seed other =
    let g = Gen.choose (-256, 256)

    let actual =
        g
        |> Gen.variant other
        |> Gen.generate seed

    let unexpected = g |> Gen.generate seed
    unexpected <>! actual

[<Theory; AutoData>]
let ``Two takes a Gen and returns a Gen of tuple``(expected : int) seed =
    let g = Gen.two (Gen.init expected)
    let actual = g |> Gen.generate seed
    (expected, expected) =! actual

[<Theory; AutoData>]
let ``Three takes a Gen and returns a Gen of triple``(expected : int) seed =
    let g = Gen.three (Gen.init expected)
    let actual = g |> Gen.generate seed
    (expected, expected, expected) =! actual

[<Theory; AutoData>]
let ``Four takes a Gen and returns a Gen of quaple`` (expected : int) seed =
    let g = Gen.four (Gen.init expected)
    let actual = g |> Gen.generate seed
    (expected, expected, expected, expected) =! actual

[<Theory; AutoData>]
let ``Both map and lift are synonyms`` seed (dummy : int) (result : int) =
    let run g = Gen.generate seed g
    let g = Gen.init dummy
    let f = fun _ -> result

    (Gen.map f g |> run) =! (Gen.lift f g |> run)

[<Theory; AutoData>]
let ``Scale adjusts the size parameter correctly`` size seed =
    let run g = Gen.generate seed g

    let actual =
        Gen.init
        |> Gen.sized
        |> Gen.scale (fun s -> s * 2)
        |> Gen.resize size
        |> run

    (size * 2) =! actual

[<Theory; AutoData>]
let ``Sample generates random values`` () =
    let actual =
        Gen.init
        |> Gen.sized
        |> Gen.sample

    let expected = Seq.length actual
    expected =! (actual
                 |> Seq.distinct
                 |> Seq.length)

[<Theory; AutoData>]
let ``SuchThatOption tries to gen a value that satisfies a predicate`` seed y =
    let run g = Gen.generate seed g
    let g = Gen.sized (fun size -> Gen.choose (-size, size))

    let actual =
        g
        |> Gen.suchThatOption (fun x -> x < y)
        |> run

    test <@ Option.isSome actual @>

[<Theory; AutoData>]
let ``SuchThat generates a value that satisfies a predicate`` seed y =
    let run g = Gen.generate seed g
    let g = Gen.sized (fun size -> Gen.choose (-size, size))

    let actual =
        g
        |> Gen.suchThat (fun x -> x < y)
        |> run

    test <@ (fun x -> x < y) actual @>

[<Theory; AutoData>]
let ``GrowingElements correctly chooses among the segments of the list`` seed =
    let sizes = [ 1..10 ]
    let run g =
        [ for n in sizes ->
              g
              |> Gen.resize n
              |> Gen.generate seed ]

    let actual = Gen.growingElements sizes |> run

    // sizes  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    // actual [1, 1, 3, 3, 4, 3, 6, 7, 3,  9]
    test <@ List.forall2 (>=) sizes actual @>

[<Theory; AutoData>]
let ``Shuffle randomly permutes a given list`` count seed (xs : Generator<int>) =
    let length = max 10 count
    let sorted =
        xs
        |> Seq.take length
        |> Seq.sort
        |> Seq.toList

    let actual =
        sorted
        |> Gen.shuffle
        |> Gen.generate seed

    let unexpected = sorted
    unexpected <>! actual

[<Theory; AutoData>]
let ``Shuffle returns an empty list when given an empty list`` seed =
    let actual =
        []
        |> Gen.shuffle
        |> Gen.generate seed

    let expected = []
    expected =! actual

[<Theory; AutoData>]
let ``SublistOf generates a random subsequence of a list`` count seed (xs : Generator<int>) =
    let length = max 10 count
    let actual =
        xs
        |> Seq.take length
        |> Gen.sublistOf
        |> Gen.generate seed
    length >! actual.Length

[<Theory; AutoData>]
let ``VectorOf generates a list of the given length`` seed size length =
    let run g = Gen.generate seed g

    let actual =
        Gen.init
        |> Gen.sized
        |> Gen.resize size
        |> Gen.vectorOf length
        |> run

    test <@ length = actual.Length && actual |> Seq.forall (fun x -> x = size) @>

[<Theory; AutoData>]
let ``ListOf generates a list of random length`` seed size =
    let run g = Gen.generate seed g

    let actual =
        Gen.choose (0, size)
        |> Gen.resize size
        |> Gen.listOf
        |> run

    test <@ actual.Length <= size && actual |> Seq.forall (fun x -> x <= size) @>

[<Theory; AutoData>]
let ``NonEmptyListOf generates a non-empty list of random length`` seed size =
    let run g = Gen.generate seed g

    let actual =
        Gen.choose (0, size)
        |> Gen.resize size
        |> Gen.nonEmptyListOf
        |> run

    test <@ actual.Length >= 0 && actual |> Seq.forall (fun x -> x <= size) @>

[<Theory; AutoData>]
let ``InfiniteSeqOf returns correct result`` seed size count =
    let run g = Gen.generate seed g

    let actual =
        Gen.choose (0, size)
        |> Gen.resize size
        |> Gen.infiniteListOf
        |> run
        |> Seq.take count
        |> Seq.toList

    test <@ count = actual.Length && actual |> Seq.forall (fun x -> x <= size) @>
