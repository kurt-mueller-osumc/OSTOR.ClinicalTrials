namespace OSTOR.ClinicalTrials.Reports

module Core =
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

            type DateOfBirth =
                internal | DateOfBirth of System.DateTime
                member this.Value = this |> fun (DateOfBirth dob) -> dob

        module Lab =
            type CliaNumber =
                internal | CliaNumber of string
                member this.Value = this |> fun (CliaNumber cliaNumber) -> cliaNumber

            type Name =
                internal | Name of string
                member this.Value = this |> fun (Name name) -> name

        type Lab =
            { Name: Lab.Name
              CliaNo: Lab.CliaNumber }

        module Patient =
            /// The patient's medical record number
            type MRN =
                internal | MRN of int64
                member this.Value = this |> fun (MRN mrn) -> mrn

            type DateOfBirth =
                internal | DateOfBirth of System.DateTime
                member this.Value = this |> fun (DateOfBirth dateOfBirth) -> dateOfBirth

        module Address =
            type StreetAddress =
                internal | StreetAddress of string
                member this.Value = this |> fun (StreetAddress streetAddress) -> streetAddress

            type City =
                internal | City of string
                member this.Value = this |> fun (City city) -> city

            type State =
                internal | State of string
                member this.Value = this |> fun (State state) -> state

            type Zipcode =
                internal | Zipcode of string
                member this.Value = this |> fun (Zipcode zipcode) -> zipcode

        type Address =
            { StreetAddress: Address.StreetAddress
              City: Address.City
              State: Address.State
              Zipcode: Address.Zipcode }

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
        module Person =
            type LastName = LastName of string

            module LastName =
                open Utilities.StringValidations

                /// Validate that last name is not blank
                let validate (LastName lastName) =
                    lastName
                    |> validateNotBlank
                    |> Result.map Domain.Person.LastName
                    |> Result.mapError (fun e -> $"Last name can't be blank")

            type FirstName = FirstName of string

            module FirstName =
                open Utilities.StringValidations

                /// Validate that a first name is not blank
                let validate (FirstName firstName) =
                    firstName
                    |> validateNotBlank
                    |> Result.map Domain.Person.FirstName
                    |> Result.mapError (fun e -> $"First name can't be blank")

            type FullName =
                { LastName: LastName
                  FirstName: FirstName }

            module FullName =
                open FsToolkit.ErrorHandling

                /// Validate that a person's first and last name is present
                let validate fullName : Validation<Domain.Person.FullName, string> =
                    validation {
                        let! lastName  = fullName.LastName |> LastName.validate
                        and! firstName = fullName.FirstName |> FirstName.validate

                        return ({ LastName = lastName
                                  FirstName = firstName } : Domain.Person.FullName)
                    }


        module Patient =
            type MRN = MRN of string

            module MRN =
                open Utilities

                /// Validate that a medical record # consists of at least one digit
                let validate (MRN mrn) : Result<Domain.Patient.MRN, string> =
                    match Integer64.tryParse mrn with
                    | Some mrn -> Ok (Domain.Patient.MRN mrn)
                    | _ -> Error $"Invalid MRN: {mrn}"

                /// Validate an MRN if it is present.
                let validateOptional (optionalMrn: MRN option) : Result<Domain.Patient.MRN option, string> =
                    optionalMrn |> Optional.validateWith validate


        type NationalProviderId = NationalProviderId of string

        module NationalProviderId =
            open Utilities

            /// Validate that an inputed national provider id is a 10 digit number.
            let validate (NationalProviderId npi) : Result<Domain.NationalProviderId,string> =
                match (String.length npi, Integer64.tryParse npi) with
                | 10, Some npi -> Ok (Domain.NationalProviderId npi)
                | _, Some npi -> Error $"NPI must be a 10 digit number: {npi}"
                | _ -> Error $"NPI is invalid: {npi}"

    module Lab =
        type CliaNumber = CliaNumber of string

        module CliaNumber =
            open System.Text.RegularExpressions


            /// Validate that a CLIA # is 10 alphanumerica numbers.
            ///
            ///    validate (Input "22D2027531") |> Ok (CliaNumber "22D2027531")
            ///    validate (Input "12345-7890") |> Error ("Invalid CLIA #: 12345-7890")
            let validate (CliaNumber cliaNumber) : Result<Domain.Lab.CliaNumber, string> =
                match Regex("^(\d|[a-zA-Z]){10}$").Match(cliaNumber).Success with
                | true -> Ok <| Domain.Lab.CliaNumber cliaNumber
                | _ -> Error $"Invalid CLIA #: {cliaNumber}. CLIA #s consist of 10 alphanumeric characters."

    type IcdCode = IcdCode of string

    module IcdCode =
        open System.Text.RegularExpressions

        /// Validate that an icd code is in the following format where 'A' is any letter and 'd' is a digit: `Add.d` or `Add.dd`
        ///
        ///     validate (Input "C34.31") = Ok (IcdCode "C34.31")
        ///     validate (Input "C11.1") = Ok (IcdCode "C11.1")
        ///     validate (Input "foobar") = Error "Icd Code is invalid: foobar"
        let validate (IcdCode icdCode) : Result<Domain.IcdCode, string> =
            if Regex("^[A-Z]\d{2}\.\d{1,2}$").Match(icdCode).Success then
                Ok (Domain.IcdCode icdCode)
            else
                Error $"Icd Code is invalid: {icdCode}"

    module Gene =
        type Name = Name of string
        module Name =
            open Utilities.StringValidations

            let (|ValidGeneName|_|) (Name name) =
                if name <> "" then Some (Name name)
                else None

            /// Validate that a gene name is not blank
            let validate (Name name) : Result<Domain.Gene.Name, string> =
                name
                |> validateNotBlank
                |> Result.map Domain.Gene.Name
                |> Result.mapError (fun _ -> $"Gene name can't be blank")