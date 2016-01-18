module LightCheck.Config

type Config =
    { MaxTest : int
      MaxFail : int
      Size    : int -> int
      Every   : int -> List<string> -> string }