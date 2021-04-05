namespace OSTOR.ClinicalTrials.Reports

module Core =
    module Types =
        module Person =
            type LastName =
                internal | LastName of string
                member this.Value = this |> fun (LastName lastName) -> lastName

            type FirstName =
                internal | FirstName of string
                member this.Value = this |> fun (FirstName firstName) -> firstName

            type FullName =
                { LastName: LastName
                  FirstName: FirstName }

            type DateOfBirth =
                internal | DateOfBirth of System.DateTime
                member this.Value = this |> fun (DateOfBirth dob) -> dob

        type City =
            internal | City of string
            member this.Value = this |> fun (City city) -> city

        type State =
            internal | State of string
            member this.Value = this |> fun (State state) -> state

        module Address =
            type StreetAddress =
                internal | StreetAddress of string
                member this.Value = this |> fun (StreetAddress streetAddress) -> streetAddress

            type ZipCode =
                internal | ZipCode of string
                member this.Value = this |> fun (ZipCode zipcode) -> zipcode

        type Address =
            { Street: Address.StreetAddress
              City: City
              State: State
              Zip: Address.ZipCode }

        module Lab =
            type CliaNumber =
                internal | CliaNumber of string
                member this.Value = this |> fun (CliaNumber cliaNumber) -> cliaNumber

            type LabName =
                internal | LabName of string
                member this.Value = this |> fun (LabName name) -> name

        type Lab =
            { CliaNumber: Lab.CliaNumber
              Name: Lab.LabName
              Address: Address }


        module Patient =
            /// The patient's medical record number
            type MRN =
                internal | MRN of int64
                member this.Value = this |> fun (MRN mrn) -> mrn

            type DateOfBirth =
                internal | DateOfBirth of System.DateTime
                member this.Value = this |> fun (DateOfBirth dateOfBirth) -> dateOfBirth


        type NationalProviderId =
            internal | NationalProviderId of int64
            member this.Value = this |> fun (NationalProviderId npi) -> npi

        /// An International Classification of Diseases code
        type IcdCode =
            internal | IcdCode of code: string
            member this.Value = this |> fun (IcdCode icdCode) -> icdCode

        // tumor mutation burden units of measure
        [<Measure>] type mutation
        [<Measure>] type megabase

        module Gene =
            type Name =
                internal | Name of string
                member this.Value = this |> fun (Name name) -> name

            type HgncId =
                internal | HgncId of string
                member this.Value = this |> fun (HgncId hgncId) -> hgncId

            type EntrezId =
                internal | EntrezId of string
                member this.Value = this |> fun (EntrezId entrezId) -> entrezId

        module Variant =
            type NucleotideAlteration =
                internal | NucleotideAlteration of string
                member this.Value = this |> fun (NucleotideAlteration na) -> na


    module Person =
        open Utilities.StringValidations.Typed
        open Types.Person

        module FirstName =
            type Input = Input of string

            /// Validate that a person's first name is not blank
            let validate (Input firstName) : Result<FirstName, string> =
                firstName |> validateNotBlank FirstName "First name can't be blank"

        module LastName =
            type Input = Input of string

            /// Validate that a person's last name is not blank
            let validate (Input input) : Result<LastName, string>=
                input |> validateNotBlank LastName "Last name can't be blank"

        module FullName =
            type Input = {
                FirstName: FirstName.Input
                LastName: LastName.Input
            }

            open FsToolkit.ErrorHandling

            /// Validate that a person's first and last name is present
            let validate (input: Input) : Validation<FullName, string> =
                validation {
                    let! lastName  = input.LastName  |> LastName.validate
                    and! firstName = input.FirstName |> FirstName.validate

                    return ({
                        LastName = lastName
                        FirstName = firstName
                    } : FullName)
                }

    module Patient =
        module MRN =
            open Utilities
            open Types.Patient

            type Input = Input of string

            /// Validate that a medical record # consists of at least one digit
            let validate (Input input) : Result<MRN, string> =
                match Integer64.tryParse input with
                | Some mrn -> Ok (MRN mrn)
                | _ -> Error $"Invalid MRN: {input}"

            /// Validate an MRN if it is present.
            let validateOptional (optionalInput: Input option) : Result<MRN option, string> =
                optionalInput |> Optional.validateWith validate

    module NationalProviderId =
        open Utilities
        open Types

        type Input = Input of string

        /// Validate that an national provider id is a 10 digit integer
        let validate (Input input) : Result<NationalProviderId,string> =
            match (String.length input, Integer64.tryParse input) with
            | 10, Some npi -> Ok (NationalProviderId npi)
            | _, Some npi -> Error $"NPI must be a 10 digit number: {npi}"
            | _ -> Error $"NPI is invalid: {input}"

    module Lab =
        module CliaNumber =
            open Types.Lab
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that a CLIA # is 10 alphanumeric characters.
            ///
            ///    validate (Input "22D2027531") |> Ok (CliaNumber "22D2027531")
            ///    validate (Input "12345-7890") |> Error ("Invalid CLIA #: 12345-7890")
            let validate (Input input) : Result<CliaNumber, string> =
                match Regex("^(\d|[a-zA-Z]){10}$").Match(input).Success with
                | true -> Ok <| CliaNumber input
                | _ -> Error $"Invalid CLIA #: {input}. CLIA #s consist of 10 alphanumeric characters."

    module IcdCode =
        open System.Text.RegularExpressions
        open Types

        type Input = Input of string

        /// Validate that an icd code is in the following format where 'A' is any letter and 'd' is a digit: `Add.d` or `Add.dd`
        ///
        ///     validate (Input "C34.31") = Ok (IcdCode "C34.31")
        ///     validate (Input "C11.1") = Ok (IcdCode "C11.1")
        ///     validate (Input "foobar") = Error "Icd Code is invalid: foobar"
        let validate (Input input) : Result<IcdCode, string> =
            if Regex("^[A-Z]\d{2}\.\d{1,2}$").Match(input).Success then
                Ok <| IcdCode input
            else
                Error $"Icd Code is invalid: {input}"

    module Gene =
        module Name =
            open Utilities.StringValidations.Typed
            open Types.Gene

            type Input = Input of string

            let (|ValidGeneName|_|) (Input input) =
                if input <> "" then Some (Name input)
                else None

            /// Validate that a gene name is not blank
            let validate (Input input) : Result<Name, string> =
                input |> validateNotBlank Name "Gene name can't be blank"