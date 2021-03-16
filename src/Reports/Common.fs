namespace OSTOR.ClinicalTrials.Reports

[<AutoOpen>]
module Common =
    type MRN =
        internal | MRN of int64

        member this.Value =
            let (MRN mrn) = this
            mrn

    type NationalProviderId =
        internal | NationalProviderId of int64

        member this.Value =
            let (NationalProviderId npi) = this
            npi

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

    type IcdCode =
        internal | IcdCode of string

        member this.Value =
            let (IcdCode icdCode) = this
            icdCode

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

        let toString fullName =
            let lastName = fullName.LastName |> LastName.toString
            let firstName = fullName.FirstName |> FirstName.toString

            $"{lastName}, {firstName}"

    module NationalProviderId =
        open Utilities

        type Input = Input of string

        /// Validate that an inputed national provider id is a 10 digit number.
        let validate (Input input) =
            match (String.length input, Integer64.tryParse input) with
            | 10, Some npi -> Ok (NationalProviderId npi)
            | _, Some npi -> Error $"NPI must be a 10 digit number: {npi}"
            | _ -> Error $"NPI is invalid {input}"

    module DateOfBirth =
        let unwrap (DateOfBirth dob) = dob

    module IcdCode =
        open System.Text.RegularExpressions

        type Input = Input of string

        /// Validate that an icd code is in the following format where 'A' is any letter and 'd' is a digit: `Add.d` or `Add.dd`
        ///
        ///     validate (Input "C34.31") = Ok (IcdCode "C34.31")
        ///     validate (Input "C11.1") = Ok (IcdCode "C11.1")
        ///     validate (Input "foobar") = Error "Icd Code is invalid: foobar"
        let validate (Input input) =
            if Regex("^[A-Z]\d{2}\.\d{1,2}$").Match(input).Success then
                Ok (IcdCode input)
            else
                Error $"Icd Code is invalid: {input}"

