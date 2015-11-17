module AutoCheck.Gen

type Gen<'a> =
    private
    | Gen of (int -> StdGen -> 'a)

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
            let (r1, r2) = split r
            let (n', mo) = (n / 2, n % 2)
            if mo = 0 then rands r1 n'
            else rands r2 n'
    Gen(fun n r -> m n (rands r v))

let generate n (Gen m) =
    let rnd = createStdGen randomSeed
    let (size, rnd') = randomR (0, n) rnd
    m size rnd'

let init a = Gen(fun n r -> a)

let bind (Gen m) f =
    Gen(fun n r0 ->
        let r1, r2 = split r0
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
