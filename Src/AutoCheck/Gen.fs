module AutoCheck.Gen

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
/// seeds to values. QuickCheck, and so AutoCheck, maintains the illusion that
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
/// <param name="v">The integer seed.</param>
let variant v (Gen m) =
    let rec rands r n =
        match n with
        | 0 -> r
        | _ ->
            let (r1, r2) = Random.split r
            let (n', mo) = (n / 2, n % 2)
            if mo = 0 then rands r1 n'
            else rands r2 n'
    Gen(fun n r -> m n (rands r v))

/// <summary>
/// Run a generator. The size passed to the generator is always 30; if you want
/// another size then you should explicitly use 'resize'.
/// </summary>
/// <param name="seed">The seed, in order to get different results on each run.
/// </param>
let generate seed (Gen m) =
    let rand = Random.create seed
    let (size, rand') = Random.range (0, 30) rand
    m size rand'

/// <summary>
/// Sequentially compose two actions, passing any value produced by the first
/// as an argument to the second.
/// </summary>
/// <param name="f">
/// The action that produces a value to be passed as argument to the generator.
/// </param>
let bind (Gen m) f =
    Gen(fun n r0 ->
        let r1, r2 = Random.split r0
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

module Operators =
    let (>>=) m f = bind m f
    let (<*>) f m = apply f m
    let (<!>) f m = map f m

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
let lift2 f m1 m2 =
    apply (apply (init f) m1) m2

/// <summary>
/// Returns a new generator obtained by applying a function to three existing
/// generators.
/// </summary>
/// <param name="f">The function to apply to the existing generators.</param>
/// <param name="m1">The existing generator.</param>
/// <param name="m2">The existing generator.</param>
/// <param name="m3">The existing generator.</param>
let lift3 f m1 m2 m3 =
    apply (apply (apply (init f) m1) m2) m3

/// <summary>
/// Returns a new generator obtained by applying a function to four existing
/// generators.
/// </summary>
/// <param name="f">The function to apply to the existing generators.</param>
/// <param name="m1">The existing generator.</param>
/// <param name="m2">The existing generator.</param>
/// <param name="m3">The existing generator.</param>
/// <param name="m4">The existing generator.</param>
let lift4 f m1 m2 m3 m4 =
    apply (apply (apply (apply (init f) m1) m2) m3) m4

let two   g = lift2 (fun a b     -> a, b)       g g
let three g = lift3 (fun a b c   -> a, b, c)    g g g
let four  g = lift4 (fun a b c d -> a, b, c, d) g g g g

/// <summary>
/// Generates a random element in the given inclusive range, uniformly distrib-
/// uted in the closed interval [lower,upper].
/// </summary>
/// <param name="lower">The lower bound.</param>
/// <param name="upper">The upper bound.</param>
let choose (lower, upper) =
    Gen (fun n r -> r) |> map (Random.range (lower, upper) >> fst)

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

let sample seed g =
    seq {
        for n in [ 0..2..20 ] -> resize n g |> generate seed
    }
