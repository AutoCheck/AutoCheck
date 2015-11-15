[<AutoOpen>]
module internal AutoCheck.Random

type StdGen =
    | StdGen of unit

let split g = (g, g)
