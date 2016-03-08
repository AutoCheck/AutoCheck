﻿/// <summary>
/// This module deals with simplifying counter-examples. A property fails when
/// LightCheck finds a first counter-example. However, randomly-generated data
/// typically contains a lot of noise. Therefore it is a good idea to simplify
/// counter-examples before reporting them. This process is called shrinking.
///
/// Read more about how it works here:
/// http://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#shrinking
/// </summary>
module LightCheck.Shrink

/// <summary>
/// A shrinker for values of type 'a.
/// </summary>
type Shrink<'a> =
    private
    | Shrink of ('a -> 'a seq)

let evaluate shrinker =
    let (Shrink func) = shrinker
    func

/// <summary>
/// Shrinks to false.
/// </summary>
/// <param name="x">The boolean value to shrink.</param>
let bool =
    function
    | true -> Seq.singleton false
    | _    -> Seq.empty
    |> Shrink

let number x =
    x
    |> Seq.unfold (fun s -> Some(x - s, s / 2))
    |> Seq.tail
    |> Seq.append [ 0 ]
    |> Seq.takeWhile (fun el -> abs x > abs el)
    |> Seq.append (if x < 0 then Seq.singleton -x
                   else Seq.empty)
    |> Seq.distinct
