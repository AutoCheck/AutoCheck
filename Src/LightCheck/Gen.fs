/// <summary>
/// LightCheck exports some basic generators, and some combinators for making
/// new ones. Gen of 'a is the type for generators of 'a's and essentially is
/// a State Monad combining a pseudo-random generation seed, and a size value
/// for data structures (i.e. list length).
/// Using the type Gen of 'a, we can specify at the same time a set of values
/// that can be generated and a probability distribution on that set.
///
/// Read more about how it works here:
/// http://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#the-gen-monad
/// http://quviq.com/documentation/eqc/index.html
/// </summary>
module LightCheck.Gen

/// <summary>
/// A generator for values of type 'a.
/// </summary>
type Gen<'a> =
    private
    | Gen of (int -> StdGen -> 'a)

/// <summary>
/// Used to construct generators that depend on the size parameter.
/// </summary>
/// <param name="g">A generator for values of type 'a.</param>
let sized g =
    Gen(fun n r ->
        let (Gen m) = g n
        m n r)

/// <summary>
/// Overrides the size parameter. Returns a generator which uses the given size
/// instead of the runtime-size parameter.
/// </summary>
/// <param name="n">The size that's going to override the runtime-size.</param>
let resize n (Gen m) = Gen(fun _ r -> m n r)

/// <summary>
/// Promotes a monadic generator to a generator of monadic values.
/// </summary>
/// <param name="f">The monadic generator </param>
/// <remarks>
/// This is an unsafe combinator for the Gen type. Gen is only morally a monad:
/// two generators that are supposed to be equal will give the same probability
/// distribution, but they might be different as functions from random number
/// seeds to values. QuickCheck, and so LightCheck, maintains the illusion that
/// a Gen is a probability distribution and does not allow you to distinguish
/// two generators that have the same distribution.
/// The promote function allows you to break this illusion by reusing the same
/// random number seed twice. This is unsafe because by applying the same seed
/// to two morally equal generators, you can see whether they are really equal
/// or not.
/// </remarks>
let promote f =
    Gen(fun n r a ->
        let (Gen m) = f a
        m n r)

/// <summary>
/// Modifies a generator using an integer seed.
/// </summary>
/// <param name="s">The integer seed.</param>
let variant s (Gen m) = Gen(fun n r -> m n (Random.variant s r))

/// <summary>
/// Runs a generator. The size passed to the generator is up to 30; if you want
/// another size then you should explicitly use 'resize'.
/// </summary>
let generate (Gen m) =
    let (size, rand) = Random.createNew() |> Random.range (0, 30)
    m size rand

/// <summary>
/// Sequentially compose two actions, passing any value produced by the first
/// as an argument to the second.
/// </summary>
/// <param name="f">
/// The action that produces a value to be passed as argument to the generator.
/// </param>
let bind (Gen m) f =
    Gen(fun n r ->
        let (r1, r2) = r |> Random.split
        let (Gen m') = f (m n r1)
        m' n r2)

/// <summary>
/// Injects a value into a generator.
/// </summary>
/// <param name="a">The value to inject into a generator.</param>
let init a = Gen(fun n r -> a)

/// <summary>
/// Unpacks a function wrapped inside a generator, applying it into a new
/// generator.
/// </summary>
/// <param name="f">The function wrapped inside a generator.</param>
/// <param name="m">The generator, to apply the function to.</param>
let apply f m =
    bind f (fun f' ->
        bind m (fun m' ->
            init (f' m')))

/// <summary>
/// Returns a new generator obtained by applying a function to an existing
/// generator.
/// </summary>
/// <param name="f">The function to apply to an existing generator.</param>
/// <param name="m">The existing generator.</param>
let map f m =
    bind m (fun m' ->
        init (f m'))

/// <summary>
/// Returns a new generator obtained by applying a function to an existing
/// generator. Synonym of map.
/// </summary>
/// <param name="f">The function to apply to an existing generator.</param>
/// <param name="m">The existing generator.</param>
let lift f m = map f m

/// <summary>
/// Returns a new generator obtained by applying a function to two existing
/// generators.
/// </summary>
/// <param name="f">The function to apply to the existing generators.</param>
/// <param name="m1">The existing generator.</param>
/// <param name="m2">The existing generator.</param>
let lift2 f m1 m2 = apply (apply (init f) m1) m2

/// <summary>
/// Returns a new generator obtained by applying a function to three existing
/// generators.
/// </summary>
/// <param name="f">The function to apply to the existing generators.</param>
/// <param name="m1">The existing generator.</param>
/// <param name="m2">The existing generator.</param>
/// <param name="m3">The existing generator.</param>
let lift3 f m1 m2 m3 = apply (apply (apply (init f) m1) m2) m3

/// <summary>
/// Returns a new generator obtained by applying a function to four existing
/// generators.
/// </summary>
/// <param name="f">The function to apply to the existing generators.</param>
/// <param name="m1">The existing generator.</param>
/// <param name="m2">The existing generator.</param>
/// <param name="m3">The existing generator.</param>
/// <param name="m4">The existing generator.</param>
let lift4 f m1 m2 m3 m4 = apply (apply (apply (apply (init f) m1) m2) m3) m4

let pair      g = lift2 (fun a b     -> a, b)       g g
let triple    g = lift3 (fun a b c   -> a, b, c)    g g g
let quadraple g = lift4 (fun a b c d -> a, b, c, d) g g g g

/// <summary>
/// Generates a random element in the given inclusive range, uniformly
/// distributed in the closed interval [lo,hi].
/// </summary>
/// <param name="lo">The lower bound.</param>
/// <param name="hi">The upper bound.</param>
let choose (lo, hi) = Gen(fun n r -> r) |> map (Random.range (lo, hi) >> fst)

/// <summary>
/// Generates one of the given values.
/// </summary>
/// <param name="xs">The input list.</param>
/// <remarks>
/// The input list must be non-empty.
/// </remarks>
let elements xs =
    // http://stackoverflow.com/a/1817654/467754
    let flip f x y = f y x
    choose (0, (Seq.length xs) - 1) |> map (flip Seq.item xs)

/// <summary>
/// Randomly uses one of the given generators.
/// </summary>
/// <param name="gens">The input list of generators to use.</param>
/// <remarks>
/// The input list must be non-empty.
/// </remarks>
let oneof gens =
    let join x = bind x id
    join (elements gens)

[<AutoOpen>]
module Builder =
    type GenBuilder() =
        member this.Bind       (m1, m2) = bind m1 m2
        member this.Return     (x)      = init x
        member this.ReturnFrom (f)      = f

    let gen = GenBuilder()

/// <summary>
/// Chooses one of the given generators, with a weighted random distribution.
/// </summary>
/// <param name="gens">The input list of tuples, in form of a weighted random
/// distribution per generator.
/// </param>
/// <remarks>
/// The input list must be non-empty.
/// </remarks>
let frequency xs =
    let upperBound = List.sumBy fst xs
    let rec pick n =
        function
        | (k, x) :: xs when n <= k -> x
        | (k, x) :: xs             -> pick (n - k) xs

    gen { let! rand = choose (1, upperBound)
          return! pick rand xs }

/// <summary>
/// Adjust the size parameter, by transforming it with the given function.
/// </summary>
/// <param name="f">The function to transform the size parameter.</param>
/// <param name="g">The generator to apply the scaling.</param>
let scale f g = sized (fun n -> resize (f n) g)

/// <summary>
/// Generates some example values.
/// </summary>
/// <param name="g">The generator to run for generating example values.</param>
let sample g =
    [ for n in [ 0..2..20 ] -> resize n g |> generate ]

/// <summary>
/// Tries to generate a value that satisfies a predicate.
/// </summary>
/// <param name="is">The predicate satisfied by the value.</param>
/// <param name="g">The generator to run for creating candidate values.</param>
let suchThatOption is g =
    let rec attempt k n =
        gen {
            match (k, n) with
            | (_, 0) -> return None
            | (k, n) ->
                let! x = resize (2 * k + n) g
                if x |> is then return Some x
                else return! attempt (k + 1) (n - 1)
        }
    sized (max 1 >> attempt 0)

/// <summary>
/// Generates a value that satisfies a predicate.
/// </summary>
/// <param name="is">The predicate satisfied by the value.</param>
/// <param name="g">The generator to run for creating candidate values.</param>
let rec suchThat is g =
    gen {
        let!  option = g |> suchThatOption is
        match option with
        | Some x -> return x
        | None   -> return! sized (fun s -> resize (s + 1) g |> suchThat is)
    }

/// <summary>
/// Takes a list of elements of increasing size, and chooses among an initial
/// segment of the list. The size of this initial segment increases, with the
/// size parameter.
/// </summary>
/// <param name="xs">The input list of elements to choose from.</param>
let growingElements xs =
    let l = Seq.length xs
    sized (fun s ->
        let s' = max 1 s
        let n  = min l s'
        elements (xs |> Seq.take n))

/// <summary>
/// Generates a random permutation of the given list.
/// </summary>
/// <param name="xs">The list to permute.</param>
let rec shuffle xs =
    let pickOne xs = xs |> List.map (fun x -> x, xs |> List.except [ x ])
    gen {
        match xs with
        | [ ] -> return []
        |  _  ->
            let! (y, ys) = xs |> pickOne |> elements
            let!     ys' = shuffle ys
            return (y :: ys')
    }

/// <summary>
/// Returns a new collection containing only the elements of the collection for
/// which the given predicate returns true when run as a generator.
/// </summary>
/// <param name="is">A function to test whether each item in the input sequence
/// should be included in the output. The result of the function is known after
/// is run as a generator.
/// </param>
/// <param name="input">The input sequence.</param>
let filter is input =
    let update x xs =
        gen {
            let! flg = is x
            let! xs' = xs
            return (if flg then x :: xs'
                    else xs')
        }
    init [] |> Seq.foldBack update input

/// <summary>
/// Generates a random subsequence of the given list.
/// </summary>
/// <param name="xs">The list to generate a random subsequence from.</param>
let sublist xs =
    filter (fun _ ->
        oneof [ init true
                init false ]) xs

/// <summary>
/// Takes a list of generators of type 'a, evaluates each one of them, and
/// collect the result, into a new generator of type 'a list.
/// </summary>
/// <param name="l">The list of generators of type 'a.</param>
/// <remarks>
/// This is written so that the F# compiler will use a tail call, as shown in
/// the resulting excerpt of generated IL:
///   IL_0000: nop
///   IL_0001: call class [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<cl...
///   IL_0006: ldarg.0
///   IL_0007: call class [FSharp.Core]Microsoft.FSharp.Collections.FSharpLi...
///   IL_000c: call class LightCheck.Gen/Gen`1<!!0> LightCheck.Gen::'init'<c...
///   IL_0011: tail.
///   IL_0013: call !!1 [FSharp.Core]Microsoft.FSharp.Collections.ListModule...
///   IL_0018: ret
/// See also:
///   http://stackoverflow.com/a/6615060/467754,
///   http://stackoverflow.com/a/35132220/467754
/// </remarks>
let sequence l =
    let k m m' =
        bind m (fun x ->
            bind m' (fun xs ->
                init (x :: xs)))
    init [] |> List.foldBack k l

/// <summary>
/// Generates a list of the given length.
/// </summary>
/// <param name="n">The number of elements to replicate.</param>
/// <param name="g">The generator to replicate.</param>
let vector n g =
    sequence [ for _ in [ 1..n ] -> g ]

/// <summary>
/// Generates a list of random length. The maximum length of the list depends
/// on the size parameter.
/// </summary>
/// <param name="g">The generator from which to create a list from.</param>
let list g = sized (fun s -> gen { let! n = choose (0, s)
                                   return! vector n g })

/// <summary>
/// Generates a non-empty list of random length. The maximum length of the list
/// depends on the size parameter.
/// </summary>
/// <param name="g">The generator from which to create a list from.</param>
let nonEmptyList g = sized (fun s -> gen { let! n = choose (1, max 1 s)
                                           return! vector n g })

/// <summary>
/// Generates a (definitely - random) unit.
/// </summary>
let unit = init()

/// <summary>
/// Generates a random byte.
/// </summary>
let byte = choose (0, 255) |> map Operators.byte

/// <summary>
/// Generates a random character.
/// </summary>
let char =
    oneof [ choose (0, 127)
            choose (0, 255) ]
    |> lift Operators.char

/// <summary>
/// Generates a random boolean.
/// </summary>
let bool =
    oneof [ init true
            init false ]

/// <summary>
/// Generates a 32-bit integer (with absolute value bounded by the generation
/// size).
/// </summary>
let int = sized (fun n -> choose (-n, n))

/// <summary>
/// Generates a 64-bit integer (with absolute value bounded by the generation
/// size multiplied by 16-bit integer's largest possible value).
/// </summary>
let int64 = int |> lift (fun n -> Operators.int64 (n * 32767))

/// <summary>
/// Generates a random real number.
/// </summary>
let float =
    let fraction a b c =
        float a + float (Operators.int b / (abs (Operators.int c) + 1))
    lift3 fraction int int int

/// <summary>
/// Generates a random real number.
/// </summary>
let double = lift ExtraTopLevelOperators.double float

/// <summary>
/// Generates a random string.
/// </summary>
let string =
    shuffle
    |> bind (list char)
    |> lift (List.toArray >> System.String)

/// <summary>
/// Generates a random real number.
/// </summary>
let decimal = lift Operators.decimal float
