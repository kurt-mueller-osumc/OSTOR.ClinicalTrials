namespace OSTOR.ClinicalTrials.Reports

[<AutoOpen>]
module Common =
    type MRN = internal MRN of string

    module MRN =
        open System.Text.RegularExpressions

        type Input = Input of string

        let validate (Input input) =
            if Regex("^(\d|[a-z]|[A-Z])+$").Match(input).Success then
                Ok (MRN input)
            else
                Error $"Invalid MRN: {input}"
