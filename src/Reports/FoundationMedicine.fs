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
                internal | SlideDeck | Block | TubeSet
                member this.Value =
                    match this with
                    | SlideDeck -> "slide deck"
                    | Block -> "block"
                    | TubeSet -> "tube set"


        /// The sample for each FMI report.
        /// FMI only reports tumor samples.
        type Sample =
            { SampleId: Sample.Identifier
              ReceivedDate: Sample.ReceivedDate
              BlockId: Sample.BlockId
              Format: Sample.Format }

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
              DateOfBirth: Person.DateOfBirth
              SpecimenSite: SpecimenSite
              CollectionDate: CollectionDate
              OrderingMd: OrderingMd
              Pathologist: Pathologist }

            member this.TryMrnValue = this.MRN |> Option.map (fun mrn -> mrn.Value)

        and Gender =
            internal Male | Female
            member this.Value =
                match this with
                | Male -> "male"
                | Female -> "female"
        and SpecimenSite = internal SpecimenSite of string
        and CollectionDate = internal CollectionDate of System.DateTime
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

        type Fusion =
            { TargetedGene: Gene.Name
              OtherGene: Gene.Name
              Description: FusionDescription
              Type: FusionType }
        and FusionDescription = FusionDescription of string
        and FusionType = FusionType of string

        module Gene =
            type Alteration = internal Alteration of alternationName : string

        type Gene =
            { Name: Gene.Name
              Alterations: Gene.Alteration list }

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
              Lab: Lab
              Genes: Gene list
              Variants: Variant list
              Fusions: Fusion list }

            member this.TryMicrosatelliteStatusValue =
                this.MicrosatelliteStatus |> Option.map (fun ms -> ms.Value)

            member this.TryTmbStatusValue =
                this.TumorMutationBurden |> Option.map (fun tmb -> tmb.Status.Value)

            member this.TryTmbScoreFloat =
                this.TumorMutationBurden |> Option.map (fun tmb -> tmb.Score.Float)

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

            /// Validate that a report id is in the following format where 'd' is a digit: `ORD-ddddddd-dd`
            ///
            ///    validate (ReportId "ORD-1234567-89") = Ok (ReportId "ORD-1234567-89")
            ///    validate (ReportId "invalidId") = Error "Invalid report id: invalidId"
            let validate input =
                if Regex("ORD-\d{7,}-\d{2,}").Match(input).Success then
                    Ok <| Domain.ReportId input
                else
                    Error <| $"Invalid report id: {input}"

        module IssuedDate =
            open Utilities

            /// Validate that a report has a valid issued date.
            let validate input =
                match DateTime.tryParse input with
                | Some issuedDate -> Ok <| Domain.IssuedDate issuedDate
                | None -> Error $"Invalid issued date: {input}"

        type Sample =
            { SampleId: string
              BlockId: string
              ReceivedDate: System.DateTime
              Format: string }

        /// A report's sample
        module Sample =
            open FsToolkit.ErrorHandling

            module SampleId =
                open System.Text.RegularExpressions

                /// Validate that a sample's id is in the following format where 'd' is a digit: `USddddddd.dd`
                ///
                ///    validate "US0123456.78" = Ok (SampleId "US0123456.78")
                ///    validate "invalidId" = Error "Invalid sample id: invalidId"
                let validate input =
                    if Regex("US\d{7,}.\d{2,}").Match(input).Success then
                        Ok <| Domain.Sample.Identifier input
                    else
                        Error <| $"Invalid sample id: {input}"

            module BlockId =
                open Utilities.StringValidations.Typed
                open type Domain.Sample.BlockId

                /// Validate that a sample's block id is not blank
                let validate = validateNotBlank BlockId "Block id can't be blank"


            module Format =
                open type Domain.Sample.Format

                /// Validate that a sample's format is either a 'Slide Deck', 'Block', and 'Tube Set'.
                let validate input =
                    match input with
                    | "Slide Deck" -> Ok SlideDeck
                    | "Block" -> Ok Block
                    | "Tube Set" -> Ok TubeSet
                    | _ -> Error $"Unknown sample format: {input}"

            open type Domain.Sample.ReceivedDate

            /// Validate the FMI report's sample
            let validate (sample: Sample) =
                validation {
                    let! sampleId = SampleId.validate sample.SampleId
                    and! blockId = BlockId.validate sample.BlockId
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
            { MRN: string
              Gender: string
              LastName: string
              FirstName: string
              SubmittedDiagnosis: string
              DateOfBirth: System.DateTime
              SpecimenSite: string
              CollectionDate: System.DateTime
              OrderingMd: OrderingMd
              Pathologist: string }

        /// The Patient Medical Information section of an FMI report
        module PMI =
            open FsToolkit.ErrorHandling

            module MRN =
                open Core.Input

                /// Validate that a patient's MRN is in the correct format or that the MRN is not provided at all.
                let validateOptional input =
                    if input = "" then
                        Ok None
                    else
                        match Patient.MRN.validate input with
                        | Ok mrn -> Ok <| Some mrn
                        | Error e -> Error e

            module Gender =
                open type Domain.Gender

                /// Validate a patient's "gender" is `(M|m)ale` or `(F|f)emale`
                let validate input =
                    match input with
                    | "male" | "Male" -> Ok Male
                    | "female" | "Female" -> Ok Female
                    | _ -> Error $"Invalid gender: {input}"

            module SubmittedDiagnosis =
                open Utilities.StringValidations.Typed
                /// Validate that submitted diagnosis name is not blank
                let validate = validateNotBlank Diagnosis.Name "Diagnosis name can't be blank"

            module SpecimenSite =
                open Utilities.StringValidations.Typed
                /// Validate that submitted diagnosis name is not blank
                let validate = validateNotBlank Domain.SpecimenSite "Diagnosis name can't be blank"

            module Pathologist =
                open Utilities.StringValidations

                /// Validate that a pathologist's name is provided or is explicitly not provided
                let validate input =
                    match input with
                    | "Provided, Not" -> Ok Domain.PathologistNotProvided
                    | NotBlank -> Ok <| Domain.PathologistName input
                    | _ -> Error "Pathologist cannot be blank"

            module OrderingMd =
                open Utilities.StringValidations
                open type Domain.OrderingMd.Name
                open type Domain.OrderingMd.Identifier

                let validate (orderingMd: OrderingMd) =
                    match orderingMd.Name, orderingMd.Identifier with
                    | NotBlank, NotBlank ->
                        Ok <| ({ Name = Name orderingMd.Name
                                 Identifier = Identifier orderingMd.Identifier } : Domain.OrderingMd)
                    | _, NotBlank -> Error "Ordering MD name can't be blank"
                    | NotBlank, _ -> Error "Ordering MD id can't be blank"
                    | _ -> Error $"Invalid ordering md: {orderingMd}"

            open Core.Domain
            open Core.Input

            /// Validate that a patient's medical information is valid
            let validate (pmi: PMI) =
                validation {
                    let! mrn = MRN.validateOptional pmi.MRN
                    and! gender = Gender.validate pmi.Gender
                    and! lastName = Person.LastName.validate pmi.LastName
                    and! firstName = Person.FirstName.validate pmi.FirstName
                    and! submittedDiagnosis = SubmittedDiagnosis.validate pmi.SubmittedDiagnosis
                    and! pathologist = Pathologist.validate pmi.Pathologist
                    and! specimenSite = SpecimenSite.validate pmi.SpecimenSite
                    and! orderingMd = OrderingMd.validate pmi.OrderingMd

                    let dob = Person.DateOfBirth pmi.DateOfBirth
                    let collectionDate = Domain.CollectionDate pmi.CollectionDate

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
            open Core.Domain

            /// Convert variant names from a comma-separated string to a list
            let (|ValidVariantNames|_|) variantNames =
                if variantNames <> "" then
                    variantNames
                    |> String.split ','
                    |> List.map Domain.VariantName
                    |> Some
                else None

            let (|ValidGeneName|_|) geneName =
                if geneName <> "" then Some (Gene.Name geneName)
                else None

            open type Domain.Variant

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
            module OtherGene =
                open Core.Domain

                let validate input =
                    match input with
                    | "N/A" | "" -> Error $"Invalid other gene for fusion: {input}"
                    | _ -> Ok (Gene.Name input)

            module TargetedGene =
                open Core.Domain
                open Utilities.StringValidations.Typed

                /// Validate that a gene name is not blank
                let validate = validateNotBlank Gene.Name $"Targeted fusion gene can't be blank"

            module Description =
                let validate (input: string) =
                    if input.Contains("fusion") then
                        Ok (Domain.FusionDescription input)
                    else
                        Error $"Invalid fusion description: {input}"

            module FusionType =
                open Utilities.StringValidations.Typed

                /// Validate that fusion type is not blank
                let validate = validateNotBlank Domain.FusionType "Fusion type cannot be blank"

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
            open type Domain.MicrosatelliteStatus
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
                open Core.Domain
                open type Domain.TumorMutationBurden.Score

                /// Validate that a tumor mutation burden score is greater than or equal to 0.0
                let validate input =
                    if input >= 0.0 then Ok <| Score (input * 1.0<mutation/megabase>)
                    else Error $"Invalid score: {input}"

            module Status =
                open type Domain.TumorMutationBurden.Status

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

        type Report =
            { ReportId: string
              IssuedDate: string
              Lab: Lab
              Sample: Sample
              PMI: PMI
              MicrosatelliteStatus: string option
              TumorMutationBurden: TumorMutationBurden option
              Genes: Gene list
              Variants: Variant list
              Fusions: Fusion list }

        module Report =
            open FsToolkit.ErrorHandling

            let validate report =
                validation {
                    let! reportId = ReportId.validate report.ReportId
                    and! sample = Sample.validate report.Sample
                    and! pmi = PMI.validate report.PMI
                    and! lab = Lab.validate report.Lab
                    and! msStatus = MicrosatelliteStatus.validateOptional report.MicrosatelliteStatus
                    and! tmb = TumorMutationBurden.validateOptional report.TumorMutationBurden
                    and! variants = Variants.validate report.Variants
                    and! issuedDate = IssuedDate.validate report.IssuedDate
                    and! genes = Genes.validate report.Genes
                    and! fusions = Fusions.validate report.Fusions

                    return ({
                        ReportId = reportId
                        Sample = sample
                        PMI = pmi
                        IssuedDate = issuedDate
                        MicrosatelliteStatus = msStatus
                        TumorMutationBurden = tmb
                        Lab = lab
                        Genes = genes
                        Variants = variants
                        Fusions = fusions
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
            member this.MicroSatelliteStatus =
                this.Genes
                |> Seq.tryFind (fun gene -> gene.Name = "Microsatellite status")
                |> Option.map (fun msStatus -> Seq.head(msStatus.Alterations))

            member this.Biomarkers =
                this.VariantReport.Biomarkers

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
            member this.Lab : Lab =
                let processSite = this.ReportSample.ProcessSites.[0]

                { Address = processSite.Address
                  CliaNumber = processSite.CliaNumber }

            /// Retrieve the report's sample
            member this.Sample : Sample =
                { SampleId = this.ReportSample.SampleId
                  BlockId = this.ReportSample.BlockId
                  ReceivedDate = this.ReportSample.ReceivedDate
                  Format = this.ReportSample.SpecFormat }

            /// Retrieve the report's patient medical information
            member this.PMI : PMI =
                let pmi = this.ClinicalReport.Pmi

                { MRN = pmi.Mrn
                  LastName = pmi.LastName
                  FirstName = pmi.FirstName
                  SubmittedDiagnosis = pmi.SubmittedDiagnosis
                  Gender = pmi.Gender
                  DateOfBirth = pmi.Dob
                  SpecimenSite = pmi.SpecSite
                  CollectionDate = pmi.CollDate
                  OrderingMd = { OrderingMd.Name = pmi.OrderingMd; OrderingMd.Identifier = pmi.OrderingMdId }
                  Pathologist = pmi.Pathologist }

            /// Retrieve the report's variants, including gene name, VUS status, and variant name(s)
            member this.Variants : Variant seq =
                this.ClinicalReport.VariantProperties
                |> Seq.map (fun variantProperty ->
                    { GeneName = variantProperty.GeneName
                      IsVus = variantProperty.IsVus
                      VariantName = variantProperty.VariantName })

            member this.Report =
                { ReportId = this.ReportId
                  IssuedDate = this.ServerTime
                  Lab = this.Lab
                  Sample = this.Sample
                  PMI = this.PMI
                  MicrosatelliteStatus = this.MicroSatelliteStatus
                  TumorMutationBurden = this.TumorMutationBurden
                  Genes = this.Genes
                  Variants = this.Variants |> Seq.toList
                  Fusions = this.Fusions |> Seq.toList }


    module DTO =
        open Database
        open Domain

        /// Build a row to be inserted into the `vendors` database table.
        let toVendorRow (overallReport: Report) =
            let row = context.Public.Vendors.Create()
            let lab = overallReport.Lab

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
                row.DateOfBirth  <- pmi.DateOfBirth.Value
                row.Sex          <- pmi.Gender.Value

                row
            )


        /// Build a row to be inserted in the `reports` database table, if the associated patient has an MRN.
        let tryReportRow (report: Report) =
            let patient = report.PMI

            patient.TryMrnValue
            |> Option.map (fun mrnValue ->
                let row = context.Public.Reports.Create()
                let lab = report.Lab

                row.PatientMrn <- mrnValue
                row.VendorCliaNumber <- lab.CliaNumber.Value
                row.ReportId <- report.ReportId.Value.ToString()
                row.OrderingPhysician <- Some patient.OrderingMd.Name.Value
                row.Pathologist <- Some patient.Pathologist.Value
                row.IssuedDate <- report.IssuedDate.Value

                row.TumorMutationalBurden <- report.TryTmbScoreFloat
                row.MsiStatus <-report.TryMicrosatelliteStatusValue

                row.DiagnosisName <- patient.SubmittedDiagnosis.Value

                row
            )

        let toTumorSampleRow (report: Report) =
            let sample = report.Sample
            let row = context.Public.Samples.Create()

            row.Category   <- "tumor"
            row.SampleId   <- sample.SampleId.Value.ToString()
            row.BiopsySite <- sample.Site.Value
            row.SampleType <- sample.Format.Value

            row

