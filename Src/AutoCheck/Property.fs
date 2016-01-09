module AutoCheck.Property

open AutoCheck.Gen

type Property =
    private
    | Prop of Gen<Result>

and Result =
    { Status : option<bool>
      Stamp  : list<string>
      Args   : list<string> }

let evaluate property =
    let (Prop result) = property
    result

let private boolProperty a =
    { Status = Some a
      Stamp  = []
      Args   = [] }
    |> Gen.init
    |> Prop

let private unitProperty =
    { Status = None
      Stamp  = []
      Args   = [] }
    |> Gen.init
    |> Prop

let private toProperty candidate =
    match box candidate with
    | :? bool as b -> boolProperty b
    | _            -> unitProperty

let forAll g f =
    Prop(gen {
             let! arg = g
             let! res = f arg
                        |> toProperty
                        |> evaluate
             return { res with Args = arg.ToString() :: res.Args }
         })
