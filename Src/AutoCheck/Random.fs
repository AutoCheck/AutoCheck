[<AutoOpen>]
module internal AutoCheck.Random

type StdGen =
    private
    | StdGen of int * int

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

let split (StdGen (s1, s2) as std) =
    let s1' = if s1 = 2147483562 then 1 else s1 + 1
    let s2' = if s2 = 1 then 2147483398 else s2 - 1
    let (StdGen (t1, t2)) = next std |> snd

    (StdGen (s1', t2), StdGen (t1, s2'))

let createStdGen i =
    let s       = i &&& 2147483647
    let (q, s1) = (s / 2147483562, s % 2147483562)
    let s2      = q % 2147483398

    StdGen (s1 + 1, s2 + 1)

let rec randomR (l, h) rng =
    if l > h then randomR (h, l) rng
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

let randomSeed () = int32 System.DateTime.UtcNow.Ticks
