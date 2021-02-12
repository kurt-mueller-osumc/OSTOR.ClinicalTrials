namespace OSTOR.ClinicalTrials.Reports

[<AutoOpen>]
module Common =
    type MRN = internal MRN of string

    type Address =
        { StreetAddress: StreetAddress
          City: City
          State: State
          Zipcode: Zipcode }

    and StreetAddress = StreetAddress of string
    and City = City of string
    and State = State of string
    and Zipcode = Zipcode of string

    module MRN =
        open System.Text.RegularExpressions

        type Input = Input of string

        let validate (Input input) =
            if Regex("^(\d|[a-z]|[A-Z])+$").Match(input).Success then
                Ok (MRN input)
            else
                Error $"Invalid MRN: {input}"
