namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    [<AutoOpen>]
    module Domain =
        open Core

        module Sample =
            type Identifier =
                internal | Identifier of string
                member this.Value = this |> fun (Identifier identifier) -> identifier

            type ReceivedDate =
                internal | ReceivedDate of System.DateTime
                member this.Value = this |> fun (ReceivedDate receivedDate) -> receivedDate

            type BlockId =
                internal | BlockId of string
                member this.Value = this |> fun (BlockId blockId) -> blockId

            type Format =
                internal | Slide | SlideDeck | Block | TubeSet
                member this.Value =
                    match this with
                    | Slide -> "slide"
                    | SlideDeck -> "slide deck"
                    | Block -> "block"
                    | TubeSet -> "tube set"

        /// The sample for each FMI report.
        /// FMI only reports tumor samples.
        type Sample =
            { SampleId: Sample.Identifier
              ReceivedDate: Sample.ReceivedDate
              BlockId: Sample.BlockId option
              Format: Sample.Format }

            member this.TryBlockIdValue =
                this.BlockId |> Option.map (fun blockId -> blockId.Value)

        module OrderingMd =
            type Name =
                internal | Name of string
                member this.Value = this |> fun (Name name) -> name

            type Identifier = internal Identifier of string

        type OrderingMd =
            { Name: OrderingMd.Name
              Identifier: OrderingMd.Identifier }

        /// Patient medical information
        type PMI =
            { MRN: Patient.MRN option
              LastName: Person.LastName
              FirstName: Person.FirstName
              SubmittedDiagnosis: Diagnosis.Name
              Gender: Gender
              DateOfBirth: Person.DateOfBirth option
              SpecimenSite: SpecimenSite option // the sample site
              CollectionDate: CollectionDate option // the sample collection date
              OrderingMd: OrderingMd
              Pathologist: Pathologist option }

            member this.TryMrnValue = this.MRN |> Option.map (fun mrn -> mrn.Value)
            member this.TryDobValue = this.DateOfBirth |> Option.map (fun dob -> dob.Value)
            member this.TryPathologistValue = this.Pathologist |> Option.map (fun pathologist -> pathologist.Value)
            member this.TrySpecimenSiteValue = this.SpecimenSite |> Option.map (fun site -> site.Value)
            member this.TryCollectionDateValue = this.CollectionDate |> Option.map (fun collectionDate -> collectionDate.Value)


        and Gender =
            internal Male | Female | Unknown
            member this.Value =
                match this with
                | Male -> "male"
                | Female -> "female"
                | Unknown -> "unknown"

        and SpecimenSite =
            internal | SpecimenSite of string
            member this.Value = this |> fun (SpecimenSite specimenSite) -> specimenSite

        and CollectionDate =
            internal | CollectionDate of System.DateTime
            member this.Value = this |> fun (CollectionDate collectionDate) -> collectionDate

        and Pathologist =
            internal | PathologistNotProvided | PathologistName of string

            member this.Value =
                match this with
                | PathologistNotProvided -> "not provided"
                | PathologistName name -> name

        type Variant =
            | VariantOfUnknownSignificance of VariantInfo
            | VariantOfKnownSignificance of VariantInfo
        and VariantInfo =
            { GeneName: Gene.Name
              VariantNames: VariantName list }
        and VariantName = internal VariantName of string

        module Fusion =
            type Description =
                internal | Description of string
                member this.Value = this |> fun (Description description) -> description
            type Type =
                internal | Type of string
                member this.Value = this |> fun (Type fusionType) -> fusionType

        type Fusion =
            { TargetedGene: Gene.Name
              OtherGene: Gene.Name
              Description: Fusion.Description
              Type: Fusion.Type }

            member this.Genes = [ this.TargetedGene; this.OtherGene]

        module Gene =
            type Alteration = internal Alteration of alternationName : string

        type Gene =
            { Name: Gene.Name
              Alterations: Gene.Alteration list }

        module ShortVariant =
            type Status =
                internal | Unknown | Known | Likely
                member this.Value =
                    match this with
                    | Unknown -> "unknown"
                    | Known -> "known"
                    | Likely -> "likely"

            type FunctionalEffect =
                internal | FunctionalEffect of string
                member this.Value = this |> fun (FunctionalEffect functionalEffect) -> functionalEffect
            type CodingSequenceEffect = internal CodingSequenceEffect of string
            type ProteinEffect =
                internal | ProteinEffect of string
                member this.Value = this |> fun (ProteinEffect proteinEffect) -> proteinEffect
            type Transcript =
                internal | Transcript of string
                member this.Value = this |> fun (Transcript transcript) -> transcript
            type AlleleFraction =
                internal | AlleleFraction of decimal
                member this.Value = this |> fun (AlleleFraction alleleFraction) -> alleleFraction

        type ShortVariant =
            { GeneName: Gene.Name
              FunctionalEffect: ShortVariant.FunctionalEffect
              Status: ShortVariant.Status
              CodingSequenceEffect: ShortVariant.CodingSequenceEffect
              ProteinEffect: ShortVariant.ProteinEffect
              Transcript: ShortVariant.Transcript
              AlleleFraction: ShortVariant.AlleleFraction }

        type MicrosatelliteStatus =
            internal
            | ``Cannot Be Determined``
            | Stable
            | ``High Instability``

            member this.Value =
                match this with
                | ``Cannot Be Determined`` -> "cannot be determined"
                | Stable -> "stable"
                | ``High Instability`` -> "high instability"

        module TumorMutationBurden =
            type Score =
                internal | Score of score: float<mutation/megabase>

                member this.Value = this |> fun (Score score) -> score
                member this.Float = float this.Value

            type Status =
                internal Low | Intermediate | High | Unknown

                member this.Value =
                    match this with
                    | Low -> "low"
                    | Intermediate -> "intermediate"
                    | High -> "high"
                    | Unknown -> "unknown"

        type TumorMutationBurden =
            { Score : TumorMutationBurden.Score
              Status: TumorMutationBurden.Status }

        type Report =
            { ReportId: ReportId
              IssuedDate: IssuedDate
              Sample: Sample
              PMI: PMI
              MicrosatelliteStatus: MicrosatelliteStatus option
              TumorMutationBurden: TumorMutationBurden option
              Labs: Lab list
              Genes: Gene list
              Variants: Variant list
              Fusions: Fusion list
              ShortVariants: ShortVariant list }

            member this.TryMicrosatelliteStatusValue =
                this.MicrosatelliteStatus |> Option.map (fun ms -> ms.Value)

            member this.TryTmbStatusValue =
                this.TumorMutationBurden |> Option.map (fun tmb -> tmb.Status.Value)

            member this.TryTmbScoreFloat =
                this.TumorMutationBurden |> Option.map (fun tmb -> tmb.Score.Float)

            static member DefaultLab = {
                CliaNumber = CliaNumber "22D2027531"
                Name = LabName "Foundation Medicine"
                Address = {
                    Street = StreetAddress "150 Second St., 1st Floor"
                    City = City "Cambridge"
                    State = State "MA"
                    Zip = ZipCode "02141"
                }
            }

            /// Grab the first lab, if it exists, or use the default FMI lab
            member this.FirstLabOrDefault =
                this.Labs
                |> Seq.tryHead
                |> Option.defaultValue Report.DefaultLab

        and ReportId =
            internal | ReportId of string
            member this.Value = this |> fun (ReportId reportId) -> reportId

        and IssuedDate =
            internal | IssuedDate of System.DateTime
            member this.Value = this |> fun (IssuedDate issuedDate) -> issuedDate

    module Input =
        open Core

        module ReportId =
            open System.Text.RegularExpressions

            let (|ValidId|_|) (input: string) =
                let idRegexes = [
                    "^ORD-\d{7,}-\d{2,}$" // e.g. `ORD-1234567-89`
                    "^CRF\d{6}$" // e.g. `CRF123456`
                    "^TRF\d{6}$" ]

                let isValidId = idRegexes |> Seq.exists (fun regex -> Regex(regex).Match(input).Success)

                if isValidId then Some <| ReportId input
                else None


            /// Validate that a report id is in a valid format.
            ///
            ///    validate "ORD-1234567-89" = Ok (ReportId "ORD-1234567-89")
            ///    validate "invalidId" = Error "Invalid report id: invalidId"
            let validate input =
                match input with
                | ValidId reportId -> Ok reportId
                | _ -> Error $"Invalid report id: {input}"

        module IssuedDate =
            open Utilities

            /// Validate that a report has a valid issued date.
            let validate input =
                match DateTime.tryParse input with
                | Some issuedDate -> Ok <| IssuedDate issuedDate
                | None -> Error $"Invalid issued date: {input}"

        type Sample =
            { SampleId: string
              BlockId: string option
              ReceivedDate: System.DateTime
              Format: string }

        /// A report's sample
        module Sample =
            open FsToolkit.ErrorHandling

            module SampleId =
                open System.Text.RegularExpressions

                let (|ValidId|_|) input =

                    let idRegexes = [
                        "US\d{7,}\.\d{2,}" // e.g. "US0123456.78"
                        "TRF\d{6}\.\d{2,}" // e.g. "TRF123456.01"
                    ]

                    let isValidId = idRegexes |> Seq.exists (fun idRegex -> Regex(idRegex).Match(input).Success)

                    if isValidId then Some <| Sample.Identifier input
                    else None


                /// Validate that a sample's id is in a valid format
                ///
                ///    validate "US0123456.78" = Ok (SampleId "US0123456.78")
                ///    validate "invalidId" = Error "Invalid sample id: invalidId"
                let validate input =
                    match input with
                    | ValidId sampleId -> Ok sampleId
                    | _ -> Error <| $"Invalid sample id: {input}"

            module BlockId =
                open Utilities
                open Utilities.StringValidations.Typed
                open type Sample.BlockId

                /// Validate that a sample's block id is not blank
                let validate = validateNotBlank BlockId "Block id can't be blank"

                let validateOptional = Optional.validateWith validate


            module Format =
                open type Sample.Format

                /// Validate that a sample's format is either a 'Slide Deck', 'Block', and 'Tube Set'.
                let validate input =
                    match input with
                    | "Slide Deck" -> Ok SlideDeck
                    | "Slide" -> Ok Slide
                    | "Block" -> Ok Block
                    | "Tube Set" -> Ok TubeSet
                    | _ -> Error $"Unknown sample format: {input}"

            open type Sample.ReceivedDate

            /// Validate the FMI report's sample
            let validate (sample: Sample) =
                validation {
                    let! sampleId = SampleId.validate sample.SampleId
                    and! blockId = BlockId.validateOptional sample.BlockId
                    and! specimenFormat = Format.validate sample.Format

                    return ({
                        SampleId = sampleId
                        ReceivedDate = ReceivedDate sample.ReceivedDate
                        BlockId = blockId
                        Format = specimenFormat } : Domain.Sample)
                }

        type OrderingMd =
            { Name: string
              Identifier: string }

        type PMI =
            { MRN: string option
              Gender: string
              LastName: string
              FirstName: string
              SubmittedDiagnosis: string
              DateOfBirth: System.DateTime option
              SpecimenSite: string option
              CollectionDate: System.DateTime option
              OrderingMd: OrderingMd
              Pathologist: string option }

        /// The Patient Medical Information section of an FMI report
        module PMI =
            open FsToolkit.ErrorHandling

            module MRN =
                open Core.Input

                /// Validate that a patient's MRN, if it exists, is valid. In this case, nonexisting MRNs include a blank string.
                let validateOptional (optionalInput: string option) =
                    match optionalInput with
                    | None -> Ok None
                    | Some input ->
                        match input with
                        | "" -> Ok None
                        | _ ->
                            input
                            |> Patient.MRN.validate
                            |> Result.map Some

            module Gender =
                open type Gender

                /// Validate a patient's "gender" is `(M|m)ale` or `(F|f)emale`
                let validate input =
                    match input with
                    | "male" | "Male" -> Ok Male
                    | "female" | "Female" -> Ok Female
                    | "Unknown" | "unknown" -> Ok Unknown
                    | _ -> Error $"Invalid gender: {input}"

            module SubmittedDiagnosis =
                open Utilities.StringValidations.Typed
                /// Validate that submitted diagnosis name is not blank
                let validate = validateNotBlank Diagnosis.Name "Diagnosis name can't be blank"

            module SpecimenSite =
                open Utilities
                open Utilities.StringValidations.Typed

                /// Validate that submitted diagnosis name is not blank
                let validate = validateNotBlank SpecimenSite "Diagnosis name can't be blank"
                let validateOptional = Optional.validateWith validate

            module Pathologist =
                open Utilities
                open Utilities.StringValidations

                /// Validate that a pathologist's name is provided or is explicitly not provided
                let validate input =
                    match input with
                    | "Provided, Not" -> Ok PathologistNotProvided
                    | NotBlank -> Ok <| PathologistName input
                    | _ -> Error "Pathologist cannot be blank"

                let validateOptional = Optional.validateWith validate

            module OrderingMd =
                open Utilities.StringValidations
                open type OrderingMd.Name
                open type OrderingMd.Identifier

                let validate (orderingMd: OrderingMd) =
                    match orderingMd.Name, orderingMd.Identifier with
                    | NotBlank, NotBlank ->
                        Ok <| ({ Name = Name orderingMd.Name
                                 Identifier = Identifier orderingMd.Identifier } : Domain.OrderingMd)
                    | _, NotBlank -> Error "Ordering MD name can't be blank"
                    | NotBlank, _ -> Error "Ordering MD id can't be blank"
                    | _ -> Error $"Invalid ordering md: {orderingMd}"

            open Core.Input

            /// Validate that a patient's medical information is valid
            let validate (pmi: PMI) =
                validation {
                    let! mrn = MRN.validateOptional pmi.MRN
                    and! gender = Gender.validate pmi.Gender
                    and! lastName = Person.LastName.validate pmi.LastName
                    and! firstName = Person.FirstName.validate pmi.FirstName
                    and! submittedDiagnosis = SubmittedDiagnosis.validate pmi.SubmittedDiagnosis
                    and! pathologist = Pathologist.validateOptional pmi.Pathologist
                    and! specimenSite = SpecimenSite.validateOptional pmi.SpecimenSite
                    and! orderingMd = OrderingMd.validate pmi.OrderingMd

                    let dob = pmi.DateOfBirth |> Option.map Person.DateOfBirth
                    let collectionDate = pmi.CollectionDate |> Option.map CollectionDate

                    return ({
                        MRN = mrn
                        Gender = gender
                        LastName = lastName
                        FirstName = firstName
                        SubmittedDiagnosis = submittedDiagnosis
                        DateOfBirth = dob
                        SpecimenSite = specimenSite
                        CollectionDate = collectionDate
                        OrderingMd = orderingMd
                        Pathologist = pathologist } : Domain.PMI)
                }

        /// A variant in an FMI report only has the gene name, whether or not it's a variant of unknown significance, and the variant name.
        type Variant =
            { GeneName: string
              IsVus: bool
              VariantName: string }

        module Variant =
            open Utilities

            /// Convert variant names from a comma-separated string to a list
            let (|ValidVariantNames|_|) variantNames =
                if variantNames <> "" then
                    variantNames
                    |> String.split ','
                    |> List.map VariantName
                    |> Some
                else None

            let (|ValidGeneName|_|) geneName =
                if geneName <> "" then Some (Gene.Name geneName)
                else None

            open type Variant

            /// Validate that a variant input has a gene name and at least one variant name.
            let validate (variant: Variant) =

                match (variant.GeneName, variant.IsVus, variant.VariantName) with
                | ValidGeneName geneName, true, ValidVariantNames variantNames ->
                    Ok <| VariantOfUnknownSignificance { GeneName = geneName
                                                         VariantNames = variantNames }
                | ValidGeneName geneName, false, ValidVariantNames variantNames ->
                    Ok <| VariantOfKnownSignificance { GeneName = geneName
                                                       VariantNames = variantNames }
                | _, _, ValidVariantNames _ -> Error $"Invalid gene name: {variant.GeneName}"
                | ValidGeneName _, _, _ -> Error $"Invalid variant names: {variant.VariantName}"
                | _ -> Error $"Invalid variant: {variant}"

        module Variants =
            open Utilities

            /// Validate a list of variant inputs
            let validate =
                List.map Variant.validate
                >> Result.combine

        type Fusion =
            { TargetedGene: string
              OtherGene: string
              Description: string
              Type: string }

        module Fusion =
            open Utilities.StringValidations.Typed

            module OtherGene =
                let validate input =
                    match input with
                    | "N/A" | "" -> Error $"Invalid other gene for fusion: {input}"
                    | _ -> Ok (Gene.Name input)

            module TargetedGene =
                /// Validate that a gene name is not blank
                let validate = validateNotBlank Gene.Name $"Targeted fusion gene can't be blank"

            module Description =
                open type Fusion.Description

                /// Validate that a fusion description contains 'fusion'
                let validate (input: string) =
                    if input.Contains("fusion") then
                        Ok (Description input)
                    else
                        Error $"Invalid fusion description: {input}"

            module FusionType =
                open type Fusion.Type

                /// Validate that fusion type is not blank
                let validate = validateNotBlank Type "Fusion type cannot be blank"

            open FsToolkit.ErrorHandling

            /// Validate that a fusion has a valid targeted gene, other gene, description, and type.
            let validate (fusion: Fusion) =
                validation {
                    let! targetedGene = fusion.TargetedGene |> TargetedGene.validate
                    and! otherGene = fusion.OtherGene |> OtherGene.validate
                    and! description = fusion.Description |> Description.validate
                    and! fusionType = fusion.Type |> FusionType.validate

                    return ({
                        TargetedGene = targetedGene
                        OtherGene = otherGene
                        Description = description
                        Type = fusionType } : Domain.Fusion )
                }

        module Fusions =
            open Utilities

            /// Validate a list of variant inputs
            let validate =
                List.map Fusion.validate
                >> Result.combine
                >> Result.mapError List.flatten

        module MicrosatelliteStatus =
            open type MicrosatelliteStatus
            open Utilities

            /// Validate that if a microsatellite status cannot be determined, is stable, oor high.
            let validate input =
                match input with
                | "Cannot Be Determined" -> Ok ``Cannot Be Determined``
                | "MS-Stable" -> Ok Stable
                | "MSI-High" -> Ok ``High Instability``
                | _ -> Error $"Invalid MicrosatelliteStatus: {input}"

            /// Validate that if a microsatellite status exists, it either cannot be determined, is stable, or has high instability
            let validateOptional = Optional.validateWith validate

        type TumorMutationBurden =
            { Score: float
              Status: string }

        module TumorMutationBurden =
            open FsToolkit.ErrorHandling

            module Score =
                open type TumorMutationBurden.Score

                /// Validate that a tumor mutation burden score is greater than or equal to 0.0
                let validate input =
                    if input >= 0.0 then Ok <| Score (input * 1.0<mutation/megabase>)
                    else Error $"Invalid score: {input}"

            module Status =
                open type TumorMutationBurden.Status

                /// Validate that a tumor mutation burden status is either low, intermediate, high, or unknown.
                let validate input =
                    match input with
                    | "low" -> Ok Low
                    | "intermediate" -> Ok Intermediate
                    | "high" -> Ok High
                    | "unknown" -> Ok Unknown
                    | _ -> Error $"Invalid tmb status: {input}"

            /// Validate that a tumor mutation burden has a valid score and status
            let validate tumorMutationBurden =
                validation {
                    let! score = Score.validate tumorMutationBurden.Score
                    and! status = Status.validate tumorMutationBurden.Status

                    return ({
                        Score = score
                        Status = status } : Domain.TumorMutationBurden)
                }

            open Utilities

            /// Validate a tumor mutation burden, if it exists
            let validateOptional = Optional.validateWith validate

        type Lab =
            { Address: string
              CliaNumber: string }

        module Lab =
            module Address =
                open System.Text.RegularExpressions

                /// Validate a FMI lab address is valid
                let validate input =
                    let regex = Regex("(?<street_address>(.)+), (?<city>[a-zA-Z]+), (?<state>[a-zA-Z]{2}) (?<zip_code>\d+)$").Match(input)

                    if regex.Success then
                        Ok ({
                            Street = (StreetAddress regex.Groups.["streetAddress"].Value)
                            City = City regex.Groups.["city"].Value
                            State = State regex.Groups.["state"].Value
                            Zip = ZipCode regex.Groups.["zip_code"].Value
                        } : Address)
                    else Error $"Invalid lab address: {input}"

            open FsToolkit.ErrorHandling
            open Core.Input

            /// Validate the report's lab address and clia number
            let validate input =
                validation {
                    let! address = Address.validate input.Address
                    and! cliaNumber = Lab.CliaNumber.validate input.CliaNumber

                    let labName = LabName "Foundation Medicine"

                    return ({
                        Name = labName
                        Address = address
                        CliaNumber = cliaNumber
                    } : Domain.Lab)
                }

        module Labs =
            open Utilities

            /// Validate a collection of labs
            let validate =
                List.map Lab.validate
                >> Result.combine
                >> Result.mapError List.flatten

        type Gene =
            { Name: string
              Alterations: string list }

        module Gene =
            open FsToolkit.ErrorHandling
            open Utilities

            module Alteration =
                open Utilities.StringValidations.Typed

                /// Validate a gene alteration is not blank
                let validate = validateNotBlank Domain.Gene.Alteration "Gene alteration can't be blank"


            module Alterations =
                /// Validate a list of gene alteration inputs. Combine all alteration validations into either a list of:
                /// - valid alterations OR
                /// - validation errors
                let validate =
                    List.map Alteration.validate >> Result.combine

            open Core.Input

            let validate gene =
                validation {
                    let! geneName = Gene.Name.validate gene.Name
                    and! geneAlterations = Alterations.validate gene.Alterations

                    return ({
                        Name = geneName
                        Alterations = geneAlterations
                    } : Domain.Gene)
                }

        module Genes =
            open Utilities

            /// Validate a list of gene inputs. Combine all gene input validations into either a list of:
            /// - valid genes OR
            /// - validation errors
            let validate =
                List.map Gene.validate
                >> Result.combine
                >> Result.mapError List.flatten

        type ShortVariant = {
            GeneName: string
            FunctionalEffect: string
            AlleleFraction: decimal
            Transcript: string
            Status: string
            ProteinEffect: string
            CodingSequenceEffect: string
        }

        module ShortVariant =
            open Utilities.StringValidations.Typed

            module FunctionalEffect =
                /// Validate that a short variant's functional effect is not blank
                let validate = validateNotBlank ShortVariant.FunctionalEffect "Functional effect cannot be blank"

            module Status =
                open type ShortVariant.Status

                /// Validate that a short variant's status is either `known`, `likely`, or `unknown`.
                let validate status =
                    match status with
                    | "known" -> Ok Known
                    | "likely" -> Ok Likely
                    | "unknown" -> Ok Unknown
                    | _ -> Error $"Invalid short variant status: {status}"

            module CdsEffect =
                /// Validate that a short variant's cds effect is not blank. Convert the `&gt;` html entity to `>`
                let validate (input: string) =
                    let replaced = input.Replace("&gt;", ">")

                    replaced |> validateNotBlank ShortVariant.CodingSequenceEffect "Short variant CDS effect cannot be blank"

            module ProteinEffect =
                /// Validate that a short variant's protein effect is not blank. Convert the `&gt;` html entity to `>`
                let validate (input: string) =
                    let replaced = input.Replace("&gt;", ">")

                    replaced |> validateNotBlank ShortVariant.ProteinEffect "Short variant protein seffect cannot be blank"

            module Transcript =
                /// Validate that a short variant's transcript is not blank
                let validate = validateNotBlank ShortVariant.Transcript "Short variant transcript cannot be blank"

            module AlleleFraction =
                open type ShortVariant.AlleleFraction

                /// Validate that a short variant's allele fraction is >= 0.0
                let validate (fraction: decimal) =
                    if fraction >= (decimal 0.0) then
                        Ok <| AlleleFraction fraction
                    else
                        Error $"Allele fraction must be >= 0.0: {fraction}"

            open FsToolkit.ErrorHandling
            open Core.Input

            /// Validate that a short variant's allele fraction, cds effect, functional effect, protein effect, gene name, status, and transcript are valid.
            let validate shortVariant =
                validation {
                    let! alleleFraction = shortVariant.AlleleFraction |> AlleleFraction.validate
                    and! cdsEffect = shortVariant.CodingSequenceEffect |> CdsEffect.validate
                    and! functionalEffect = shortVariant.FunctionalEffect |> FunctionalEffect.validate
                    and! geneName = shortVariant.GeneName |> Gene.Name.validate
                    and! proteinEffect = shortVariant.ProteinEffect |> ProteinEffect.validate
                    and! status = shortVariant.Status |> Status.validate
                    and! transcript = shortVariant.Transcript |> Transcript.validate

                    return ({
                        GeneName = geneName
                        FunctionalEffect = functionalEffect
                        ProteinEffect = proteinEffect
                        CodingSequenceEffect = cdsEffect
                        Transcript = transcript
                        AlleleFraction = alleleFraction
                        Status = status
                    } : Domain.ShortVariant)
                }

        module ShortVariants =
            open Utilities

            /// Validate a collection of short variants
            let validate =
                List.map ShortVariant.validate
                >> Result.combine
                >> Result.mapError List.flatten


        type Report =
            { ReportId: string
              IssuedDate: string
              Labs: Lab list
              Sample: Sample
              PMI: PMI
              MicrosatelliteStatus: string option
              TumorMutationBurden: TumorMutationBurden option
              Genes: Gene list
              Variants: Variant list
              Fusions: Fusion list
              ShortVariants: ShortVariant list }

        module Report =
            open FsToolkit.ErrorHandling

            let validate report =
                validation {
                    let! reportId = ReportId.validate report.ReportId
                    and! sample = Sample.validate report.Sample
                    and! pmi = PMI.validate report.PMI
                    and! labs = Labs.validate report.Labs
                    and! msStatus = MicrosatelliteStatus.validateOptional report.MicrosatelliteStatus
                    and! tmb = TumorMutationBurden.validateOptional report.TumorMutationBurden
                    and! variants = Variants.validate report.Variants
                    and! issuedDate = IssuedDate.validate report.IssuedDate
                    and! genes = Genes.validate report.Genes
                    and! fusions = Fusions.validate report.Fusions
                    and! shortVariants = ShortVariants.validate report.ShortVariants

                    return ({
                        ReportId = reportId
                        Sample = sample
                        PMI = pmi
                        IssuedDate = issuedDate
                        MicrosatelliteStatus = msStatus
                        TumorMutationBurden = tmb
                        Labs = labs
                        Genes = genes
                        Variants = variants
                        Fusions = fusions
                        ShortVariants = shortVariants
                    } : Domain.Report)
                }

    module XML =
        open FSharp.Data
        open System.IO
        open System.Xml.Linq
        open Input

        [<Literal>]
        let ClinicalReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/clinicalReport.xsd"
        [<Literal>]
        let VariantReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/variantReport.xsd"

        type ClinicalReportProvider = XmlProvider<Schema=ClinicalReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.clinicalReport.xsd">
        type VariantReportProvider = XmlProvider<Schema=VariantReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.variantReport.xsd">

        /// A Foundation Report XML
        type Report(filePath: string) =
            let filePath = filePath
            let xmlText = File.ReadAllText(filePath)
            let xml = XDocument.Parse(xmlText)
            let rrNamespace = XNamespace.Get "http://integration.foundationmedicine.com/reporting"
            let variantNamespace = XNamespace.Get "http://foundationmedicine.com/compbio/variant-report-external"
            let resultsReportName = rrNamespace + "ResultsReport"
            let resultsPayloadName = rrNamespace + "ResultsPayload"
            let finalReportName = XName.Get "FinalReport"
            let variantReportName = variantNamespace + "variant-report"

            let finalReportElement = xml.Element(resultsReportName)
                                        .Element(resultsPayloadName)
                                        .Element(finalReportName)

            let variantReportElement = xml.Element(resultsReportName)
                                          .Element(resultsPayloadName)
                                          .Element(variantReportName)

            member _.ClinicalReport = ClinicalReportProvider.Parse(finalReportElement.ToString())
            member _.VariantReport = VariantReportProvider.Parse(variantReportElement.ToString())

            member this.Biomarkers = this.VariantReport.Biomarkers
            member this.ReportSample = this.ClinicalReport.Sample

            /// When the report was issued
            member this.ServerTime =
                this.ClinicalReport.Signature.ServerTime

            /// The `Genes` section of the XML report. Each `Gene` has a `Name` and  many `Alterations`, with their own `Name`s.
            member this.Genes : Gene list =
                this.ClinicalReport.Genes
                |> Seq.map (fun gene ->
                    { Name = gene.Name
                      Alterations = gene.Alterations |> Array.toList |> List.map (fun alteration -> alteration.Name)
                    }
                ) |> Seq.toList

            /// Retrieve the report's microsatellite status, if it exists.
            member this.MicrosatelliteStatus =
                this.Biomarkers.MicrosatelliteInstability
                |> Option.map (fun msStatus -> msStatus.Status)

            member this.TumorMutationBurden : TumorMutationBurden option =
                this.Biomarkers.TumorMutationBurden
                |> Option.map (fun tmb ->
                    { Score = float tmb.Score
                      Status = tmb.Status })

            member this.Fusions =
                this.VariantReport.Rearrangements
                |> Seq.filter (fun rearrangement -> rearrangement.Description.Contains("fusion"))
                |> Seq.map (fun fusion ->
                    { TargetedGene = fusion.TargetedGene
                      OtherGene = fusion.OtherGene
                      Description = fusion.Description
                      Type = fusion.Type }
                )

            member this.ReportId = this.ClinicalReport.ReportId

            /// Retrieve the lab's address and clia number
            member this.Labs : Lab seq =
                this.ReportSample.ProcessSites
                |> Seq.map (fun processSite ->
                    { Address = processSite.Address
                      CliaNumber = processSite.CliaNumber })


            /// Retrieve the report's sample
            member this.Sample : Sample =
                /// a block id is optional
                let optionalBlockId =
                    try (Some this.ReportSample.BlockId)
                    with | :? System.Exception -> None

                { SampleId = this.ReportSample.SampleId
                  BlockId = optionalBlockId
                  ReceivedDate = this.ReportSample.ReceivedDate
                  Format = this.ReportSample.SpecFormat }

            /// Retrieve the report's patient medical information
            member this.PMI : PMI =
                let pmi = this.ClinicalReport.Pmi

                let optionalPathologist =
                    try (Some pmi.Pathologist)
                    with | :? System.Exception -> None

                let optionalSpecSite =
                    try (Some pmi.SpecSite)
                    with | :? System.Exception -> None

                let optionalMrn =
                    try (Some pmi.Mrn)
                    with | :? System.Exception -> None

                let optionalCollectionDate =
                    try (Some pmi.CollDate)
                    with | :? System.Exception -> None

                let optionalDob =
                    try (Some pmi.Dob)
                    with | :? System.Exception -> None

                { MRN = optionalMrn
                  LastName = pmi.LastName
                  FirstName = pmi.FirstName
                  SubmittedDiagnosis = pmi.SubmittedDiagnosis
                  Gender = pmi.Gender
                  DateOfBirth = optionalDob
                  SpecimenSite = optionalSpecSite
                  CollectionDate = optionalCollectionDate
                  OrderingMd = { OrderingMd.Name = pmi.OrderingMd; OrderingMd.Identifier = pmi.OrderingMdId }
                  Pathologist = optionalPathologist }

            /// Retrieve the report's variants, including gene name, VUS status, and variant name(s)
            member this.Variants : Variant seq =
                this.ClinicalReport.VariantProperties
                |> Seq.map (fun variantProperty ->
                    { GeneName = variantProperty.GeneName
                      IsVus = variantProperty.IsVus
                      VariantName = variantProperty.VariantName })


            /// the short variants sections will include the same gene and alteration name found in the gene/alteration section
            member this.ShortVariants =
                this.VariantReport.ShortVariants
                |> Seq.filter (fun shortVariant -> shortVariant.Status <> "unknown")
                |> Seq.map (fun shortVariant ->
                    { AlleleFraction = shortVariant.AlleleFraction
                      FunctionalEffect = shortVariant.FunctionalEffect
                      GeneName = shortVariant.Gene
                      CodingSequenceEffect = shortVariant.CdsEffect
                      ProteinEffect = shortVariant.ProteinEffect // also the alteration name found in gene alterations
                      Transcript = shortVariant.Transcript
                      Status = shortVariant.Status
                    })

            member this.Report =
                { ReportId = this.ReportId
                  IssuedDate = this.ServerTime
                  Labs = this.Labs |> Seq.toList
                  Sample = this.Sample
                  PMI = this.PMI
                  MicrosatelliteStatus = this.MicrosatelliteStatus
                  TumorMutationBurden = this.TumorMutationBurden
                  Genes = this.Genes
                  Variants = this.Variants |> Seq.toList
                  Fusions = this.Fusions |> Seq.toList
                  ShortVariants = this.ShortVariants |> Seq.toList }


    module DTO =
        open Database
        open Domain

        /// Build a row to be inserted into the `vendors` database table.
        let toVendorRow (report: Report) =
            let row = context.Public.Vendors.Create()
            let lab = report.FirstLabOrDefault

            row.Name          <- "Foundation Medicine"
            row.CliaNumber    <- lab.CliaNumber.Value
            row.StreetAddress <- lab.Address.Street.Value
            row.City          <- lab.Address.City.Value
            row.State         <- lab.Address.State.Value
            row.ZipCode       <- lab.Address.Zip.Value

            row



        /// Build a row to be inserted into the `patients` database table if the report's patient has an MRN.
        let tryPatientRow (report: Report) =
            let pmi = report.PMI

            pmi.TryMrnValue
            |> Option.map (fun mrnValue ->
                let row = context.Public.Patients.Create()

                row.Mrn          <- mrnValue
                row.LastName     <- pmi.LastName.Value
                row.FirstName    <- pmi.FirstName.Value
                row.DateOfBirth  <- pmi.DateOfBirth.Value.Value // assume that the optional dob exists and get the underlying datetime
                row.Sex          <- pmi.Gender.Value

                row
            )


        /// Build a row to be inserted in the `reports` database table, if the associated patient has an MRN.
        let tryReportRow (report: Report) =
            let patient = report.PMI

            patient.TryMrnValue
            |> Option.map (fun mrnValue ->
                let row = context.Public.Reports.Create()
                let lab = report.FirstLabOrDefault

                row.PatientMrn <- mrnValue
                row.VendorCliaNumber <- lab.CliaNumber.Value
                row.ReportId <- report.ReportId.Value.ToString()
                row.OrderingPhysician <- Some patient.OrderingMd.Name.Value
                row.Pathologist <- patient.TryPathologistValue
                row.IssuedDate <- report.IssuedDate.Value

                row.TumorMutationalBurden <- report.TryTmbScoreFloat
                row.MsiStatus <-report.TryMicrosatelliteStatusValue

                row.DiagnosisName <- patient.SubmittedDiagnosis.Value

                row
            )

        let toSampleRow (report: Report) =
            let sample = report.Sample
            let pmi = report.PMI
            let row = context.Public.Samples.Create()

            row.Category   <- "tumor"
            row.SampleId   <- sample.SampleId.Value
            row.BiopsySite <- pmi.TrySpecimenSiteValue
            row.SampleType <- sample.Format.Value

            row

        let toSampleReportRow (report: Report) =
            let row = context.Public.SampleReports.Create()
            let sample = report.Sample
            let pmi = report.PMI

            row.ReportId <- report.ReportId.Value
            row.SampleId <- sample.SampleId.Value
            row.BlockId <- sample.TryBlockIdValue

            row.CollectionDate <- pmi.TryCollectionDateValue
            row.ReceiptDate    <- sample.ReceivedDate.Value

            row

        let toGeneRows (report: Report) =
            let shortGeneNames = report.ShortVariants |> List.map (fun shortVariant -> shortVariant.GeneName)
            let fusionGenes = report.Fusions |> List.collect (fun fusion -> fusion.Genes)

            shortGeneNames
            @ fusionGenes
            |> List.map (fun geneName ->
                let row = context.Public.Genes.Create()

                row.Name <- geneName.Value

                row
            )

        let querySampleReportId (reportId: ReportId) (sampleId: Sample.Identifier) =
            query {
                for sampleReport in context.Public.SampleReports do
                where (sampleReport.ReportId = reportId.Value.ToString() && sampleReport.SampleId = sampleId.Value.ToString())
                select sampleReport.Id
                exactlyOne
            }

        let toVariantRows (sampleReportId: System.Guid) (report: Report) =
            report.ShortVariants
            |> Seq.map (fun shortVariant ->
                let row = context.Public.Variants.Create()

                row.SampleReportId <- sampleReportId
                row.GeneName <- shortVariant.GeneName.Value
                row.Name     <- shortVariant.ProteinEffect.Value
                row.Category <- "somatic"

                row.Type <- shortVariant.FunctionalEffect.Value |> Some
                row.Assessment <- shortVariant.Status.Value |> Some

                row.Transcript <- shortVariant.Transcript.Value |> Some
                row.AllelicFraction <- shortVariant.AlleleFraction.Value |> float |> Some

                row
            )

        let toFusionRows (sampleReportId: System.Guid) (report: Report) =
            report.Fusions |> Seq.map (fun fusion ->
                let row = context.Public.Fusions.Create()

                row.SampleReportId <- sampleReportId
                row.FirstGeneName <- fusion.TargetedGene.Value
                row.SecondGeneName <- fusion.OtherGene.Value

                row.Description <- fusion.Description.Value |> Some
                row.FusionType <- fusion.Type.Value

                row
            )

        open Utilities

        let tryDatabaseRows (report: Report) =
            report |> tryPatientRow |> Option.map (fun patientRow ->
                /// insert patient, vendor, genes, and sample into database
                report |> toVendorRow |> ignore
                report |> toGeneRows |> ignore
                report |> toSampleRow |> ignore

                context.SubmitUpdates()

                /// insert report into database
                report |> tryReportRow |> Optional.value |> ignore

                context.SubmitUpdates()

                /// insert sample report, variants, and fusions into database
                let sampleReportId = querySampleReportId report.ReportId report.Sample.SampleId
                report |> toVariantRows sampleReportId |> ignore
                report |> toFusionRows sampleReportId |> ignore

                context.SubmitUpdates()
            )
