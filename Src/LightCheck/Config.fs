module LightCheck.Config

open System

type Config =
    { MaxTest : int
      MaxFail : int
      Size    : int -> int
      Every   : int -> List<string> -> string }

let quick =
    { MaxTest = 100
      MaxFail = 1000
      Size    = fun n -> 3 + n / 2
      Every   = fun n args -> sprintf "%s\b" (n.ToString()) }

let verbose =
    { quick with Every =
                     fun n args ->
                         let s = n.ToString()
                         let a = ":" + Environment.NewLine + String.Concat(args)
                         s + a }

let defaultConfig = quick
