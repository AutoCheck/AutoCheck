module LightCheck.Runner

open LightCheck.Gen
open LightCheck.Property
open LightCheck.Config

let check config property =
    let done' mesg ntest stamps =
        let display xs =
            match xs with
            | [   ] -> ".\n"
            | [ x ] -> " (" + x + ").\n"
            |   _   -> ".\n" + System.String.Concat(xs |> Seq.map ((+) "."))

        let pairLength l =
            match l with
            | (xs :: _) as xss -> List.length xss, xs

        let percentage n m = ((100 * n) / m).ToString() + "%"
        let entry (n, xs)  = (percentage n ntest) + " " + (String.concat ", " xs)

        // http://stackoverflow.com/a/23929245/467754
        let groupConsecutive sq =
            (Array.ofSeq sq, []) ||> Array.foldBack (fun x ->
                                         function
                                         | [] -> [ [ x ] ]
                                         | xs :: xss ->
                                             if x = List.head xs then (x :: xs) :: xss
                                             else [ x ] :: xs :: xss)

        let table =
            display
            << List.map entry
            << List.sortBy id
            << List.map pairLength
            << groupConsecutive
            << List.sort
            << List.filter (not << function | [] -> true | _ -> false)
        System.Console.WriteLine(mesg + " " + ntest.ToString() + " tests" + table stamps)

    let rec tests config g ntest nfail stamps =
        if   ntest = config.MaxTest then done' "OK, passed" ntest stamps
        elif nfail = config.MaxFail then done' "Arguments exhausted after" ntest stamps
        else
            let result = Gen.generate (Gen.resize ntest g)
            System.Console.WriteLine(config.Every ntest (result.Args))

            match result.Status with
            | None       -> tests config g ntest (nfail + 1) stamps
            | Some true  -> tests config g (ntest + 1) nfail (result.Stamps :: stamps)
            | Some false ->
                System.Console.WriteLine
                    ("Falsifiable, after "
                        + (ntest.ToString())
                        + " tests:\n"
                        + System.String.Concat(result.Args))

    tests config (Property.evaluate property) 0 0 []
