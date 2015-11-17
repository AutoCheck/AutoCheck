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
