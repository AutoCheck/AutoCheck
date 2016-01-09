module AutoCheck.UnitTests.PropertyTests

open Xunit
open Swensen.Unquote

open AutoCheck
open AutoCheck.Property

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
