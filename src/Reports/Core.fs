namespace OSTOR.ClinicalTrials.Reports

module Core =
    [<AutoOpen>]
    module Domain =
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
                member this.Value =
                    $"{this.LastName.Value}, {this.FirstName.Value}"

            type DateOfBirth =
                internal | DateOfBirth of System.DateTime
                member this.Value = this |> fun (DateOfBirth dob) -> dob

        type City =
            internal | City of string
            member this.Value = this |> fun (City city) -> city

        type State =
            internal | State of string
            member this.Value = this |> fun (State state) -> state

        type StreetAddress =
            internal | StreetAddress of string
            member this.Value = this |> fun (StreetAddress streetAddress) -> streetAddress

        type ZipCode =
            internal | ZipCode of string
            member this.Value = this |> fun (ZipCode zipcode) -> zipcode

        type Address =
            { Street: StreetAddress
              City: City
              State: State
              Zip: ZipCode }

        type CliaNumber =
            internal | CliaNumber of string
            member this.Value = this |> fun (CliaNumber cliaNumber) -> cliaNumber

        type LabName =
            internal | LabName of string
            member this.Value = this |> fun (LabName name) -> name

        type Lab =
            { CliaNumber: CliaNumber
              Name: LabName
              Address: Address }

        module Diagnosis =
            type Name =
                internal | Name of string
                member this.Value = this |> fun (Name name) -> name

        module Patient =
            /// The patient's medical record number
            type MRN =
                internal | MRN of int64
                member this.Value = this |> fun (MRN mrn) -> mrn

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

    module Input =
        module StreetAddress =
            open Utilities.StringValidations.Typed

            /// Validate that a street address is not blank
            let validate =
                validateNotBlank Domain.StreetAddress "Street address cannot be lank"

        module Person =
            open Utilities.StringValidations.Typed
            open Domain.Person

            module FirstName =
                /// Validate that a person's first name is not blank
                let validate = validateNotBlank FirstName "First name can't be blank"

            module LastName =
                /// Validate that a person's last name is not blank
                let validate = validateNotBlank LastName "Last name can't be blank"

            type FullName = {
                FirstName: string
                LastName: string
            }

            module FullName =
                open FsToolkit.ErrorHandling

                /// Validate that a person's first and last name is present
                let validate (fullName: FullName) : Validation<Domain.Person.FullName, string> =
                    validation {
                        let! lastName  = fullName.LastName  |> LastName.validate
                        and! firstName = fullName.FirstName |> FirstName.validate

                        return ({
                            LastName = lastName
                            FirstName = firstName
                        } : Domain.Person.FullName)
                    }

        module Patient =
            module MRN =
                open Utilities
                open Domain.Patient

                /// Validate that a medical record # consists of at least one digit
                let validate (str: string) : Result<MRN, string> =
                    match Integer64.tryParse str with
                    | Some mrn -> Ok (MRN mrn)
                    | _ -> Error $"Invalid MRN: {str}"

                /// Validate an MRN if it is present.
                let validateOptional (optionalString: string option) : Result<MRN option, string> =
                    optionalString |> Optional.validateWith validate

        module NationalProviderId =
            open Domain
            open System.Text.RegularExpressions

            let (|ValidNPI|_|) input =
                if Regex("^\d{9,}$").Match(input).Success then
                    Some <| NationalProviderId (int64 input)
                else None

            /// Validate that an national provider id is at least a 9 digit integer
            let validate (str: string) : Result<NationalProviderId,string> =
                match str with
                | ValidNPI npi -> Ok npi
                | _ -> Error $"Invalid NPI: {str}"

        module Lab =
            open Domain

            module Name =
                open Utilities.StringValidations.Typed

                /// Validate that lab name is present
                let validate =
                    validateNotBlank LabName "Lab name can't be blank"

            module CliaNumber =
                open System.Text.RegularExpressions

                /// Validate that a CLIA # is 10 alphanumeric characters.
                ///
                ///    validate "22D2027531" |> Ok (CliaNumber "22D2027531")
                ///    validate "12345-7890" |> Error ("Invalid CLIA #: 12345-7890")
                let validate (str: string) : Result<CliaNumber, string> =
                    match Regex("^(\d|[a-zA-Z]){10}$").Match(str).Success with
                    | true -> Ok <| CliaNumber str
                    | _ -> Error $"Invalid CLIA #: {str}. CLIA #s consist of 10 alphanumeric characters."

        module IcdCode =
            open System.Text.RegularExpressions
            open Domain

            /// Validate that an icd code is in the following format where 'A' is any letter and 'd' is a digit: `Add.d` or `Add.dd`
            ///
            ///     validate "C34.31" = Ok (IcdCode "C34.31")
            ///     validate "C11.1" = Ok (IcdCode "C11.1")
            ///     validate "foobar" = Error "Icd Code is invalid: foobar"
            let validate (str: string) : Result<IcdCode, string> =
                if Regex("^[A-Z]\d{2}\.\d{1,2}$").Match(str).Success then
                    Ok <| IcdCode str
                else
                    Error $"Icd Code is invalid: {str}"

        module Gene =
            module Name =
                open Utilities.StringValidations.Typed
                open Domain.Gene

                let (|ValidGeneName|_|) str =
                    if str <> "" then Some (Name str)
                    else None

                /// Validate that a gene name is not blank
                let validate =
                    validateNotBlank Name "Gene name can't be blank"

        module Report =
            type ValidationError =
                { ReportId: string
                  Errors: string list }