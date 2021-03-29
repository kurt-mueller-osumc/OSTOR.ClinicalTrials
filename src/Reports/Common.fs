namespace OSTOR.ClinicalTrials.Reports

[<AutoOpen>]
module Common =
    /// The patient's medical record number
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

    type LabCliaNumber =
        internal | LabCliaNumber of string

        member this.Value = this |> fun (LabCliaNumber clia) -> clia

    type FullName =
        { LastName: LastName
          FirstName: FirstName }
    and LastName =
        internal | LastName of string
        member this.Value = this |> fun (LastName lastName) -> lastName
    and FirstName =
        internal | FirstName of string
        member this.Value = this |> fun (FirstName firstName) -> firstName

    type DateOfBirth =
        internal | DateOfBirth of System.DateTime

        member this.Value = this |> fun (DateOfBirth dob) -> dob

    /// An International Classification of Diseases code
    type IcdCode =
        internal | IcdCode of code: string

        member this.Value =
            let (IcdCode icdCode) = this
            icdCode

    type Gene =
        { Name: GeneName }
    and GeneName =
        internal | GeneName of string

        member this.Value = this |> fun (GeneName geneName) -> geneName

    // tumor mutation burden units of measure
    [<Measure>] type mutation
    [<Measure>] type megabase

    type ReportId =
        internal
        | ReportId of string

        member this.Value = this |>fun (ReportId reportId) -> reportId

    module ReportId =
        type Input = Input of string

    module MRN =
        open Utilities

        type Input = Input of string

        /// Validate that a medical record # consists of at least one digit
        let validate (Input input) =
            match Integer64.tryParse input with
            | Some mrn -> Ok (MRN mrn)
            | _ -> Error $"MRN - Invalid MRN: ({input})"

        /// Validate an MRN if it is present.
        let validateOptional (optionalInput: Input option) =
            match optionalInput with
            | None -> Ok None
            | Some input ->
                match validate input with
                | Ok mrn -> Ok <| (Some mrn)
                | Error e -> Error e

        let toInteger (mrn: MRN) = mrn.Value

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

    module Lab =
        module CliaNumber =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that a CLIA # is 10 alphanumerica numbers.
            ///
            ///    validate (Input "22D2027531") |> Ok (CliaNumber "22D2027531")
            ///    validate (Input "12345-7890") |> Error ("Invalid CLIA #: 12345-7890")
            let validate (Input input) =
                if Regex("^(\d|[a-zA-Z]){10}$").Match(input).Success then
                    Ok <| LabCliaNumber input
                else
                    Error $"Invalid CLIA #: {input}. CLIA #s consist of 10 alphanumeric characters."

    module Diagnosis =
        type NameInput = NameInput of string

        type Name =
            internal | Name of string

            member this.Value = this |> fun (Name name) -> name

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

        let toString (icdCode: IcdCode) = icdCode.Value

    module Gene =
        module Name =
            open Utilities.StringValidations

            type Input = Input of string

            let (|ValidGeneName|_|) (Input input) =
                if input <> "" then Some (GeneName input)
                else None

            /// Validate that a gene name is not blank.
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map GeneName
                |> Result.mapError (fun _ -> $"Gene name can't be blank")