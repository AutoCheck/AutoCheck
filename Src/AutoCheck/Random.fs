[<AutoOpen>]
module internal AutoCheck.Random

open System

type StdGen =
    private
    | StdGen of int * int

let split g = (g, g)
let createStdGen i = StdGen(i, i)
let randomSeed () = int32 DateTime.UtcNow.Ticks
let randomR a g = (fst a, g)
