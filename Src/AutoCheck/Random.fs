[<AutoOpen>]
module internal AutoCheck.Random

type StdGen =
    private
    | StdGen of int * int

let split g = (g, g)
