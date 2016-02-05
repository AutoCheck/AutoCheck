namespace LightCheck

open LightCheck.Gen

/// <summary>
/// Represents a pair of a generator and a shrinker, used for random generation
/// and (optional) shrinking of values.
/// </summary>
[<AbstractClass>]
type Arbitrary<'a>() =

    /// <summary>
    /// A generator for values of the given type.
    /// </summary>
    abstract Generator : Gen<'a>

    /// <summary>
    /// Produces a (possibly) empty list of all the possible immediate shrinks
    /// of the given value. The default implementation returns the empty list,
    /// so will not try to shrink the value.
    /// Most implementations of shrink should try at least three things:
    ///  - Shrink a term to any of its immediate subterms.
    ///  - Recursively apply shrink to all immediate subterms.
    ///  - Type-specific shrinkings such as replacing a type by a simpler ones.
    /// </summary>
    abstract      Shrink   : 'a -> 'a seq
    override this.Shrink _ = Seq.empty

type ArbitraryUnit() =
    inherit Arbitrary<unit>()

    /// <summary>
    /// Generates a (definitely - random) unit.
    /// </summary>
    override this.Generator = init()

type ArbitraryByte() =
    inherit Arbitrary<byte>()

    /// <summary>
    /// Generates a random byte.
    /// </summary>
    override this.Generator = choose (0, 256) |> lift byte

type ArbitraryChar() =
    inherit Arbitrary<char>()

    /// <summary>
    /// Generates a random character.
    /// </summary>
    override this.Generator =
        oneof [ choose (0, 127)
                choose (0, 255) ]
        |> lift char

type ArbitraryBool() =
    inherit Arbitrary<bool>()

    /// <summary>
    /// Generates a random boolean.
    /// </summary>
    override this.Generator =
        oneof [ init true
                init false ]

type ArbitraryInt() =
    inherit Arbitrary<int>()

    /// <summary>
    /// Generates a 32-bit integer (with the absolute value bounded by the
    /// generation size).
    /// </summary>
    override this.Generator = sized (fun n -> choose (-n, n))

type ArbitraryInt64() =
    inherit Arbitrary<int64>()

    /// <summary>
    /// Generates a 64-bit integer (with absolute value bounded by the
    /// generation size multiplied by 16-bit integer's largest possible value).
    /// </summary>
    override this.Generator =
        ArbitraryInt().Generator
        |> lift (fun n -> int64 (n * 32767))

type ArbitraryString() =
    inherit Arbitrary<string>()

    /// <summary>
    /// Generates a random string.
    /// </summary>
    override this.Generator =
        shuffle
        |> bind (list (ArbitraryChar()).Generator)
        |> lift (List.toArray >> System.String)

type ArbitraryFloat() =
    inherit Arbitrary<float>()

    /// <summary>
    /// Generates a random real number.
    /// </summary>
    override this.Generator =
        let fraction a b c = float a + float (int b / (abs (int c) + 1))
        let g = ArbitraryInt().Generator
        lift3 fraction g g g

type ArbitraryDouble() =
    inherit Arbitrary<double>()

    /// <summary>
    /// Generates a random real number.
    /// </summary>
    override this.Generator =
        ArbitraryFloat().Generator
        |> lift double

type ArbitraryDecimal() =
    inherit Arbitrary<decimal>()

    /// <summary>
    /// Generates a random real number.
    /// </summary>
    override this.Generator =
        ArbitraryFloat().Generator
        |> lift decimal
