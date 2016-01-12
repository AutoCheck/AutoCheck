﻿module LightCheck.Tests.PropertyTests

open Xunit
open Swensen.Unquote

open LightCheck
open LightCheck.Property

open Ploeh.AutoFixture.Xunit
open Xunit.Extensions

[<Fact>]
let ``ForAll returns correct result for properties that are true`` () =
    let g = Gen.list Gen.int32
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
    let g = Gen.list Gen.int32
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
    let g = Gen.int32
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
let ``Label adds expected information to properties`` expected =
    let g = Gen.list Gen.int32
    let prop = fun xs -> xs |> List.rev |> List.rev = xs

    let actual =
        prop
        |> Property.forAll g
        |> Property.label expected
        |> Property.evaluate
        |> Gen.sample

    test <@ actual
            |> Seq.map (fun r -> r.Stamp)
            |> Seq.concat
            |> Seq.forall (fun actual -> expected = actual) @>
