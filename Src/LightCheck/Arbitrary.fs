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

