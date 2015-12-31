/// <summary>
/// This module deals with the common task of pseudo-random number generation.
/// It makes it possible to generate repeatable results, by starting with a
/// specified initial random number generator, or to get different results on
/// echch run by using the system-initialised generator or by supplying a seed
/// from some other source.
/// </summary>
/// <remarks>
/// This implementation uses the Portable Combined Generator of L'Ecuyer for
/// 32-bit computers, transliterated by Lennart Augustsson. It has a period of
/// roughly 2.30584e18.
/// </remarks>
[<AutoOpen>]
module internal AutoCheck.Random

type StdGen =
    private
    | StdGen of int * int

/// <summary>
/// The next operation returns an Int that is uniformly distributed in the
/// rangge of at least 30 bits, and a new generator. The result of repeatedly
/// using next should be at least as statistically robust as the Minimal
/// Standard Random Number Generator. Until more is known about implementations
/// of split, all we require is that split deliver generators that are (a) not
/// identical and (b) independently robust in the sense just given.
/// </summary>
let private next (StdGen (s1, s2)) =
    let k    = s1 / 53668
    let k'   = s2 / 52774
    let s1'  = 40014 * (s1 - k * 53668)  - k * 12211
    let s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
    let s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    let s2'' = if s2' < 0 then s2' + 2147483399 else s2'
    let z    = s1'' - s2''
    let z'   = if z < 1 then z + 2147483562 else z

    (z', StdGen (s1'', s2''))

/// <summary>
/// The split operation allows one to obtain two distinct random number
/// generators. This is very useful in functional programs (for example, when
/// passing a random number generator down to recursive calls), but very little
/// work has been done on statistically robust implementations of split.
/// </summary>
let split (StdGen (s1, s2) as std) =
    let s1' = if s1 = 2147483562 then 1 else s1 + 1
    let s2' = if s2 = 1 then 2147483398 else s2 - 1
    let (StdGen (t1, t2)) = next std |> snd

    (StdGen (s1', t2), StdGen (t1, s2'))

/// <summary>
/// The create operation provides a way of producing an initial generator by
/// mapping an Int into a generator. Distinct arguments should be likely to
/// produce distinct generators.
/// </summary>
/// <param name="seed">The seed, in order to get different results on each run.
/// </param>
let create seed =
    let s       = seed &&& 2147483647
    let (q, s1) = (s / 2147483562, s % 2147483562)
    let s2      = q % 2147483398

    StdGen (s1 + 1, s2 + 1)

/// <summary>
/// The range operation takes a range (lo,hi) and a random number generator g,
/// and returns a random value, uniformly distributed, in the closed interval
/// [lo,hi], together with a new generator.
/// </summary>
/// <remarks>
/// It is unspecified what happens if lo > hi. For continuous types there is no
/// requirement that the values lo and hi are ever produced, although they very
/// well may be, depending on the implementation and the interval.
/// </remarks>
let rec range (l, h) rng =
    if l > h then range (h, l) rng
    else
        let (l', h')    = (32767, 2147483647)
        let b           = h' - l' + 1
        let q           = 1000
        let k           = h - l + 1
        let magnitude   = k * q
        let rec f c v g =
            if c >= magnitude then (v, g)
            else
                let (x, g') = next g
                let v'      = (v * b + (x - l'))
                f (c * b) v' g'
        let (v, rng') = f 1 0 rng

        (l + v % k), rng'

/// <summary>
/// Creates a new StdGen instance, which is a modified version of the supplied
/// StdGen isntance. The returned StdGen instance is modified using the integer
/// seed that is being supplied.
/// </summary>
/// <param name="n">The integer seed.</param>
/// <param name="r">
/// The initial StdGen instance from which to return a modified one.
/// </param>
let rec variant n r =
    match n with
    | 0 -> r
    | _ ->
        let rand = split r
        let seed = n / 2
        if n % 2 = 0
        then (fst rand) |> variant seed
        else (snd rand) |> variant seed
