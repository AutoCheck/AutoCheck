/// <summary>
/// This module deals with simplifying counter-examples. A property fails when
/// LightCheck finds a first counter-example. However, randomly-generated data
/// typically contains a lot of noise. Therefore it is a good idea to simplify
/// counter-examples before reporting them. This process is called shrinking.
///
/// Read more about how it works here:
/// http://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#shrinking
/// </summary>
module LightCheck.Shrink

open FSharp.Core.LanguagePrimitives

/// <summary>
/// A shrinker for values of type 'a.
/// </summary>
type Shrink<'a> =
    private
    | Shrink of ('a -> 'a seq)

/// <summary>
/// Shrinks to false.
/// </summary>
/// <param name="x">The boolean value to shrink.</param>
let bool =
    function
    | true -> Seq.singleton false
    | _    -> Seq.empty

/// <summary>
/// Shrinks towards smaller numeric values.
/// </summary>
/// <param name="n">The numeric value to shrink.</param>
let inline number n =
    let genericTwo = GenericOne + GenericOne
    n
    |> Seq.unfold (fun s -> Some(n - s, s / genericTwo))
    |> Seq.tail
    |> Seq.append [ GenericZero ]
    |> Seq.takeWhile (fun el -> abs n > abs el)
    |> Seq.append (if n < GenericZero then Seq.singleton -n
                   else Seq.empty)
    |> Seq.distinct

/// <summary>
/// Injects a function of type 'a -> 'a seq into a shrinker.
/// </summary>
/// <param name="f">The function to inject into a shrinker.</param>
let init f = Shrink f

/// <summary>
/// Returns a value of type 'a -> 'a seq out of a shrinker.
/// </summary>
/// <param name="f">
/// The union case from where the shrink function must be returned.
/// </param>
let evaluate (Shrink f) = f

let list shrink xs =
    seq {
        yield []
        for x in xs do
            for y in shrink x do
                yield [ y ]
    }
