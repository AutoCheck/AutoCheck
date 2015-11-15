module AutoCheck.Gen

type Gen<'a> =
    private
    | Gen of (int -> StdGen -> 'a)

let sized g =
    Gen(fun n r ->
        let (Gen m) = g n
        m n r)

let resize n (Gen m) = Gen(fun _ r -> m n r)
