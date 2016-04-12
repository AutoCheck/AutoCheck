module LightCheck.Tests.PropertyExamples

open Xunit
open Swensen.Unquote

open LightCheck
open LightCheck.Property

open Ploeh.AutoFixture.Xunit
open Xunit.Extensions

[<Fact>]
let ``ForAll returns correct result for properties that are true`` () =
    let g = Gen.list Gen.int
    let prop = fun xs -> xs |> List.rev |> List.rev = xs

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Status)
            |> Seq.choose id
            |> Seq.forall id @>

[<Fact>]
let ``ForAll returns correct result for properties that are false`` () =
    let g = Gen.list Gen.int
    let prop = fun xs -> xs |> List.rev = xs

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Status)
            |> Seq.choose id
            |> Seq.forall id
            |> not @>

[<Fact>]
let ``Implies returns correct result when the precondition is true`` () =
    let g = Gen.int
    let prop a = a <> 0 ==> lazy (1/a = 1/a)

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Status)
            |> Seq.choose id
            |> Seq.forall id @>

[<Theory; AutoData>]
let ``Label labels a test case`` expected =
    let g = Gen.list Gen.int
    let prop = fun xs -> xs |> List.rev |> List.rev = xs

    let actual =
        prop
        |> Property.forAll g
        |> Property.label expected
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Stamps)
            |> Seq.concat
            |> Seq.forall (fun actual -> expected = actual) @>

[<Theory; AutoData>]
let ``Classify conditionally labels a test case`` x y expected =
    let g =
        Gen.frequency [ (100, Gen.init x)
                        (200, Gen.init y) ]
    let prop a =
        a <> 0 ==> lazy (1/a = 1/a) |> Property.classify (a <> y) expected

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Stamps)
            |> Seq.concat
            |> Seq.exists (fun actual -> expected = actual) @>

[<Theory; AutoData>]
let ``Trivial conditionally labels a test case`` x y =
    let g =
        Gen.frequency [ (100, Gen.init x)
                        (200, Gen.init y) ]
    let prop a =
        a <> 0 ==> lazy (1/a = 1/a) |> Property.trivial (a <> y)

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Stamps)
            |> Seq.concat
            |> Seq.exists (fun actual -> "trivial" = actual) @>

[<Theory; AutoData>]
let ``Collect gathers all values that are passed to it`` x y =
    let g =
        Gen.frequency [ (100, Gen.init x)
                        (200, Gen.init y) ]
    let prop a =
        a <> 0 ==> lazy (1/a = 1/a) |> Property.collect a

    let actual =
        prop
        |> Property.forAll g
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Stamps)
            |> Seq.concat
            |> Seq.exists (fun actual ->
                string x = actual ||
                string y = actual) @>
