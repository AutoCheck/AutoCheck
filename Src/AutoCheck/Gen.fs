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

let resize n (Gen m) = Gen(fun _ r -> m n r)

let promote f =
    Gen(fun n r a ->
        let (Gen m) = f a
        m n r)

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

let generate n seed (Gen m) =
    let rand = Random.create seed
    let (size, rand') = Random.range (0, n) rand
    m size rand'

let init a = Gen(fun n r -> a)

let bind (Gen m) f =
    Gen(fun n r0 ->
        let r1, r2 = Random.split r0
        let (Gen m') = f (m n r1)
        m' n r2)

let apply f m =
    bind f (fun f' ->
        bind m (fun m' ->
            init (f' m')))

let map f m =
    bind m (fun m' ->
        init (f m'))

let map2 f m1 m2 =
    apply (apply (init f) m1) m2

let map3 f m1 m2 m3 =
    apply (apply (apply (init f) m1) m2) m3

let map4 f m1 m2 m3 m4 =
    apply (apply (apply (apply (init f) m1) m2) m3) m4

module Operators =
    let (>>=) m f = bind m f
    let (<*>) f m = apply f m
    let (<!>) f m = map f m

[<AutoOpen>]
module Builder =
    type GenBuilder() =
        member this.Bind       (m1, m2) = bind m1 m2
        member this.Combine    (m1, m2) = bind m1 (fun () -> m2)
        member this.Delay      (f)      = bind (init()) f
        member this.Return     (x)      = init x
        member this.ReturnFrom (f)      = f
        member this.Zero       ()       = init()

    let gen = GenBuilder()

/// <summary>
/// Generates a random element in the given inclusive range, uniformly distrib-
/// uted in the closed interval [lower,upper].
/// </summary>
/// <param name="lower">The lower bound.</param>
/// <param name="upper">The upper bound.</param>
let choose (lower, upper) =
    Gen (fun n r -> r) |> map (Random.range (lower, upper) >> fst)

let elements xs =
    // http://stackoverflow.com/a/1817654/467754
    let flip f x y = f y x
    choose (0, (Seq.length xs) - 1) |> map (flip Seq.item xs)

let oneof gens =
    let join x = bind x id
    join (elements gens)

let frequency xs =
    let upperBound = List.sumBy fst xs
    let rec pick n =
        function
        | (k, x) :: xs when n <= k -> x
        | (k, x) :: xs             -> pick (n - k) xs

    gen { let! rand = choose (1, upperBound)
          return! pick rand xs }

let two   g = map2 (fun a b     -> a, b)       g g
let three g = map3 (fun a b c   -> a, b, c)    g g g
let four  g = map4 (fun a b c d -> a, b, c, d) g g g g
