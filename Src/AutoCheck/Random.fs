[<AutoOpen>]
module internal AutoCheck.Random

open System

type StdGen =
    private
    | StdGen of int * int

let split (StdGen (s1, s2) as std) =
    let next (StdGen (s1, s2)) =
        let k    = s1 / 53668
        let k'   = s2 / 52774
        let s1'  = 40014 * (s1 - k * 53668)  - k * 12211
        let s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
        let s1'' = if s1' < 0 then s1' + 2147483563 else s1'
        let s2'' = if s2' < 0 then s2' + 2147483399 else s2'
        let z    = s1'' - s2''
        let z'   = if z < 1 then z + 2147483562 else z
        (z', StdGen (s1'', s2''))
    let s1' = if s1 = 2147483562 then 1 else s1 + 1
    let s2' = if s2 = 1 then 2147483398 else s2 - 1
    let (StdGen (t1, t2)) = next std |> snd

    (StdGen (s1', t2), StdGen (t1, s2'))

let createStdGen i = StdGen(i, i)
let randomSeed () = int32 DateTime.UtcNow.Ticks
let randomR a g = (fst a, g)
