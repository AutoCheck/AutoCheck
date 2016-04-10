module LightCheck.Tests.GenExamples

open Swensen.Unquote

open LightCheck
open LightCheck.Gen

open Ploeh.AutoFixture
open Ploeh.AutoFixture.Xunit
open Xunit.Extensions

[<Theory; AutoData>]
let ``Choose produces elements in range`` (size : int) =
    let upper =  size |> System.Math.Abs
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
let ``Sized passes the current size`` (size : int) =
    let g = Gen.sized (fun s -> Gen.choose (-s, s))

    let actual = Gen.generate g

    let upper =  size |> System.Math.Abs
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
let ``Resize overrides the size parameter`` newSize size =
    newSize <>! size
    let g = Gen.sized Gen.init |> Gen.resize newSize

    let actual = g |> Gen.generate

    newSize =! actual

[<Theory; AutoData>]
let ``Oneof randomly uses one of the given generators`` () =
    let g1 = Gen.init 1
    let g2 = Gen.init 2
    let g3 = Gen.init 3

    let actual = Gen.oneof [ g1; g2; g3 ] |> Gen.sample

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

    let actual =
        Gen.frequency [ (100, g1)
                        (200, g2) ]
        |> Gen.sample
        |> Seq.countBy id
        |> Seq.sortBy id
        |> Seq.toList

    let g1s = actual |> List.item 0 |> snd
    let g2s = actual |> List.item 1 |> snd
    test <@ g1s < g2s @>

[<Theory; AutoData>]
let ``Return in gen workflow returns correct result`` (s : string) =
    let actual = gen { return s } |> Gen.generate
    let expected = Gen.init s |> Gen.generate
    expected =! actual

[<Theory; AutoData>]
let ``Variant modifies a generator using an integer seed`` seed =
    let g = Gen.choose (-256, 256)

    let actual =
        g
        |> Gen.variant seed
        |> Gen.generate

    let unexpected = g |> Gen.generate
    unexpected <>! actual

[<Theory; AutoData>]
let ``Pair takes a Gen and returns a Gen of tuple``(expected : int) =
    let g = Gen.pair (Gen.init expected)
    let actual = g |> Gen.generate
    (expected, expected) =! actual

[<Theory; AutoData>]
let ``Triple takes a Gen and returns a Gen of triple``(expected : int) =
    let g = Gen.triple (Gen.init expected)
    let actual = g |> Gen.generate
    (expected, expected, expected) =! actual

[<Theory; AutoData>]
let ``Quadraple takes a Gen and returns a Gen of quaple`` (expected : int) =
    let g = Gen.quadraple (Gen.init expected)
    let actual = g |> Gen.generate
    (expected, expected, expected, expected) =! actual

[<Theory; AutoData>]
let ``Both map and lift are synonyms`` (dummy : int) (result : int) =
    let g = Gen.init dummy
    let f = fun _ -> result

    (Gen.map f g |> Gen.generate) =! (Gen.lift f g |> Gen.generate)

[<Theory; AutoData>]
let ``Scale adjusts the size parameter correctly`` size =
    let actual =
        Gen.init
        |> Gen.sized
        |> Gen.scale (fun s -> s * 2)
        |> Gen.resize size
        |> Gen.generate

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
let ``SuchThatOption tries to gen a value that satisfies a predicate`` y =
    let g = Gen.sized (fun size -> Gen.choose (-size, size))

    let actual =
        g
        |> Gen.suchThatOption (fun x -> x < y)
        |> Gen.generate

    test <@ Option.isSome actual @>

[<Theory; AutoData>]
let ``SuchThat generates a value that satisfies a predicate`` y =
    let g = Gen.sized (fun size -> Gen.choose (-size, size))

    let actual =
        g
        |> Gen.suchThat (fun x -> x < y)
        |> Gen.generate

    test <@ (fun x -> x < y) actual @>

[<Theory; AutoData>]
let ``GrowingElements correctly chooses among the segments of the list`` =
    let sizes = [ 1..10 ]
    let run g =
        [ for n in sizes ->
              g
              |> Gen.resize n
              |> Gen.generate ]

    let actual = Gen.growingElements sizes |> run

    // sizes  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    // actual [1, 1, 3, 3, 4, 3, 6, 7, 3,  9]
    test <@ List.forall2 (>=) sizes actual @>

[<Theory; AutoData>]
let ``Shuffle randomly permutes a given list`` count (xs : Generator<int>) =
    let length = max 10 count
    let sorted =
        xs
        |> Seq.take length
        |> Seq.sort
        |> Seq.toList

    let actual =
        sorted
        |> Gen.shuffle
        |> Gen.generate

    let unexpected = sorted
    unexpected <>! actual

[<Theory; AutoData>]
let ``Shuffle returns an empty list when given an empty list`` =
    let actual =
        []
        |> Gen.shuffle
        |> Gen.generate

    let expected = []
    expected =! actual

[<Theory; AutoData>]
let ``Sublist generates a random subsequence of a list`` count (xs : Generator<int>) =
    let length = max 10 count
    let actual =
        xs
        |> Seq.take length
        |> Gen.sublist
        |> Gen.generate
    length >! actual.Length

[<Theory; AutoData>]
let ``Vector generates a list of the given length`` size length =
    let actual =
        Gen.init
        |> Gen.sized
        |> Gen.resize size
        |> Gen.vector length
        |> Gen.generate

    test <@ length = actual.Length && actual |> Seq.forall (fun x -> x = size) @>

[<Theory; AutoData>]
let ``List generates a list of random length`` size =
    let actual =
        Gen.choose (0, size)
        |> Gen.resize size
        |> Gen.list
        |> Gen.generate

    test <@ actual.Length <= size && actual |> Seq.forall (fun x -> x <= size) @>

[<Theory; AutoData>]
let ``NonEmptyList generates a non-empty list of random length`` size =
    let actual =
        Gen.choose (0, size)
        |> Gen.resize size
        |> Gen.nonEmptyList
        |> Gen.generate

    test <@ actual.Length >= 0 && actual |> Seq.forall (fun x -> x <= size) @>
