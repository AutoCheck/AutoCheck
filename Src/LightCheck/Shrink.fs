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
    |> Shrink

(*
-- | Shrink an integral number.
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x =
  nub $
  [ -x
  | x < 0, -x > x
  ] ++
  [ x'
  | x' <- takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
  ]
 where
   -- a << b is "morally" abs a < abs b, but taking care of overflow.
   a << b = case (a >= 0, b >= 0) of
            (True,  True)  -> a < b
            (False, False) -> a > b
            (True,  False) -> a + b < 0
            (False, True)  -> a + b > 0
*)

let quot a b = System.Math.DivRem(int a, int b) |> fst
