namespace OSTOR.ClinicalTrials.Reports

[<AutoOpen>]
module Common =
    type MRN = internal MRN of int64
    type NationalProviderId = internal NationalProviderId of string

    type Address =
        { StreetAddress: StreetAddress
          City: City
          State: State
          Zipcode: Zipcode }
    and StreetAddress = internal StreetAddress of string
    and City = internal City of string
    and State = internal State of string
    and Zipcode = internal Zipcode of string

    type FullName =
        { LastName: LastName
          FirstName: FirstName }
    and LastName = internal LastName of string
    and FirstName = internal FirstName of string

    type DateOfBirth = DateOfBirth of System.DateTime

    module MRN =
        open Utilities

        type Input = Input of string

        /// Validate that a medical record # consists of at least one digit or one alphabet letter
        let validate (Input input) =
            match Integer64.tryParse input with
            | Some mrn -> Ok (MRN mrn)
            | _ -> Error $"MRN - Invalid MRN: ({input})"

        /// Validate an MRN if it is present.
        let validateOptional (Input input) =
            if input = "" then
                Ok None
            else
                (Input input)
                |> validate
                |> Result.map Some

        let toInteger (MRN mrn) = mrn

    module LastName =
        open Utilities.StringValidations

        type Input = Input of string

        /// Validate that last name is not blank
        let validate (Input input) =
            input
            |> validateNotBlank
            |> Result.map LastName
            |> Result.mapError (fun e -> $"LastName: {e}")

        let toString (LastName lastName) = lastName

    module FirstName =
        open Utilities.StringValidations

        type Input = Input of string

        /// Validate that a first name is not blank
        let validate (Input input) =
            input
            |> validateNotBlank
            |> Result.map FirstName
            |> Result.mapError (fun e -> $"FirstName: {e}")

        let toString (FirstName firstName) = firstName

    module FullName =
        open FsToolkit.ErrorHandling

        type Input =
            { LastNameInput: LastName.Input
              FirstNameInput: FirstName.Input }

        /// Validate that a person's first and last name is present
        let validate input =
            validation {
                let! lastName = input.LastNameInput |> LastName.validate
                and! firstName = input.FirstNameInput |> FirstName.validate

                return { LastName = lastName
                         FirstName = firstName }
            }

    module NationalProviderId =
        open System.Text.RegularExpressions

        type Input = Input of string

        let validate (Input input) =
            if Regex("^\d{10}$").Match(input).Success then
                Ok <| NationalProviderId input
            else Error $"NationalProviderId - Invalid id: {input}"

    module DateOfBirth =
        let unwrap (DateOfBirth dob) = dob