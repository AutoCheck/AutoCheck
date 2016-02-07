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
