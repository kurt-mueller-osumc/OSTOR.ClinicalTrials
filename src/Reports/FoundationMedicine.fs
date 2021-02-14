namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    [<Measure>] type mutation
    [<Measure>] type megabase

    /// The sample for each FMI report
    type Sample =
        { SampleId: SampleId
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          SampleFormat: SampleFormat }
    and SampleId = internal | SampleId of string
    and ReceivedDate = internal | ReceivedDate of System.DateTime
    and BlockId = internal | BlockId of string
    and SampleFormat = internal | SlideDeck | Block | TubeSet

    type PMI =
        { MRN: MRN option
          LastName: LastName
          FirstName: FirstName
          SubmittedDiagnosis: SubmittedDiagnosis
          Gender: Gender
          DateOfBirth: DateOfBirth
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate
          OrderingMd: OrderingMd
          Pathologist: Pathologist }
    and LastName = internal LastName of string
    and FirstName = internal FirstName of string
    and SubmittedDiagnosis = internal SubmittedDiagnosis of string
    and Gender = internal Male | Female
    and DateOfBirth = internal DateOfBirth of System.DateTime
    and SpecimenSite = internal SpecimenSite of string
    and CollectionDate = internal CollectionDate of System.DateTime
    and OrderingMd =
        { MdName: OrderingMdName
          MdId: OrderingMdId }
    and OrderingMdName = internal OrderingMdName of string
    and OrderingMdId = internal OrderingMdId of string
    and Pathologist = internal Pathologist of string

    type Variant =
        | VariantOfUnknownSignificance of VariantInfo
        | VariantOfKnownSignificance of VariantInfo
    and VariantInfo =
        { GeneName: GeneName
          VariantNames: VariantName list }
    and GeneName = internal GeneName of string
    and VariantName = internal VariantName of string

    type Gene =
        { GeneName: GeneName
          GeneAlterations: GeneAlteration list }
    and GeneAlteration =
        { AlterationName: GeneAlterationName
          Interpretation: GeneAlterationInterpretation }
    and GeneAlterationName = internal GeneAlterationName of name: string
    and GeneAlterationInterpretation = internal GeneAlterationInterpretation of interpretation: string

    type MicrosatelliteStatus =
        internal
        | ``Cannot Be Determined``
        | Stable
        | ``High Instability``

    type TumorMutationBurden =
        { Score : TmbScore
          Status: TmbStatus }
    and TmbScore = internal TmbScore of score: float<mutation/megabase>
    and TmbStatus = internal Low | Intermediate | High | UnknownStatus

    type Lab =
        { Address: Address
          CliaNumber: CliaNumber }
    and CliaNumber = CliaNumber of string

    type Report =
        { ReportId: ReportId
          IssuedDate: IssuedDate
          Sample: Sample
          PMI: PMI
          MicrosatelliteStatus: MicrosatelliteStatus option
          TumorMutationBurden: TumorMutationBurden option
          Lab: Lab
          Variants: Variant list }
    and ReportId = internal ReportId of string
    and IssuedDate = IssuedDate of System.DateTime

    module ReportId =
        open System.Text.RegularExpressions

        type Input = Input of string

        /// Validate that a report id is in the following format where 'd' is a digit: ORD-ddddddd-dd
        ///
        ///    validate (ReportId "ORD-1234567-89") = Ok (ReportId "ORD-1234567-89")
        ///    validate (ReportId "invalidId") = Error "Invalid report id: invalidId"
        let validate (Input reportId) =
            if Regex("ORD-\d{7,}-\d{2,}").Match(reportId).Success then
                Ok <| ReportId reportId
            else
                Error <| $"Invalid report id: {reportId}"

    module IssuedDate =
        open Utilities
        type Input = Input of string

        let validate (Input input) =
            match DateTime.tryParse input with
            | Some issuedDate -> Ok <| IssuedDate issuedDate
            | None -> Error $"Invalid issued date: {input}"

    /// A report's sample
    module Sample =
        open FsToolkit.ErrorHandling

        module SampleId =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that a sample's id is in the following format where 'd' is a digit: USddddddd.dd
            ///
            ///    validate (SampleId "US0123456.78") = Ok (SampleId "US0123456.78")
            ///    validate (SampleId "invalidId") = Error "Invalid sample id: invalidId"
            let validate (Input input) =
                if Regex("US\d{7,}.\d{2,}").Match(input).Success then
                    Ok <| SampleId input
                else
                    Error <| $"Invalid sample id: {input}"

        module BlockId =
            type Input = Input of string

            /// Validate that a block id is not blank.
            let validate (Input input) =
                if input <> "" then
                    Ok <| BlockId input
                else
                    Error $"Invalid block id: {input}"

        module SampleFormat =
            type Input = Input of string

            /// Validate that a sample's specimen format is not blank.
            let validate (Input input) =
                match input with
                | "Slide Deck" -> Ok SlideDeck
                | "Block" -> Ok Block
                | "Tube Set" -> Ok TubeSet
                | _ -> Error $"Unknown sample format: {input}"

        type Input =
            { SampleIdInput: SampleId.Input
              ReceivedDate: ReceivedDate
              BlockId: BlockId.Input
              SampleFormat: SampleFormat.Input }

        /// Validate a sample from a FMI report
        let validate (sampleInput: Input) =
            validation {
                let! sampleId = SampleId.validate sampleInput.SampleIdInput
                and! blockId = BlockId.validate sampleInput.BlockId
                and! specimenFormat = SampleFormat.validate sampleInput.SampleFormat

                return { SampleId = sampleId
                         ReceivedDate = sampleInput.ReceivedDate
                         BlockId = blockId
                         SampleFormat = specimenFormat }
            }

    /// Patient Medical Information
    module PMI =
        open FsToolkit.ErrorHandling

        module MRN =
            let validate (MRN.Input input) =
                if input = "" then
                    Ok None
                else
                    MRN.Input input
                    |> MRN.validate
                    |> Result.map Some

        module Gender =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "male" | "Male" -> Ok Male
                | "female" | "Female" -> Ok Female
                | _ -> Error $"Invalid gender: {input}"

        type Input =
            { MrnInput: MRN.Input
              GenderInput: Gender.Input
              LastName: LastName
              FirstName: FirstName
              SubmittedDiagnosis: SubmittedDiagnosis
              DateOfBirth: DateOfBirth
              SpecimenSite: SpecimenSite
              CollectionDate: CollectionDate
              OrderingMd: OrderingMd
              Pathologist: Pathologist }

        let validate (pmiInput: Input) =
            validation {
                let! mrn = MRN.validate pmiInput.MrnInput
                and! gender = Gender.validate pmiInput.GenderInput

                return { MRN = mrn
                         Gender = gender
                         LastName = pmiInput.LastName
                         FirstName = pmiInput.FirstName
                         SubmittedDiagnosis = pmiInput.SubmittedDiagnosis
                         DateOfBirth = pmiInput.DateOfBirth
                         SpecimenSite = pmiInput.SpecimenSite
                         CollectionDate = pmiInput.CollectionDate
                         OrderingMd = pmiInput.OrderingMd
                         Pathologist = pmiInput.Pathologist }
            }

    module Variant =
        /// A variant in an FMI report only has the gene name, whether or not it's a variant of unknown significance, and the variant name.
        type Input =
            { GeneName: GeneName
              IsVus: IsVus
              VariantName: VariantName }
        and IsVus = IsVus of bool

        module Input =
            open Utilities

            /// Convert variant names from a comma-separated string to a list
            let (|ValidVariantNames|_|) (VariantName variantName) =
                if variantName <> "" then
                    variantName
                    |> String.split ','
                    |> List.map VariantName
                    |> Some

                else None

            let (|ValidGeneName|_|) (GeneName geneName) =
                if geneName <> "" then Some (GeneName geneName)
                else None

            /// Validate that a variant input has a gene name and at least one variant name.
            let validate (variantInput: Input) =
                let (IsVus isVus) = variantInput.IsVus

                match (variantInput.GeneName, isVus, variantInput.VariantName) with
                | ValidGeneName geneName, true, ValidVariantNames variantNames -> Ok <| VariantOfUnknownSignificance { GeneName = geneName; VariantNames = variantNames }
                | ValidGeneName geneName, false, ValidVariantNames variantNames -> Ok <| VariantOfKnownSignificance { GeneName = geneName; VariantNames = variantNames }
                | _, _, ValidVariantNames _ -> Error $"Invalid gene name: {variantInput.GeneName}"
                | ValidGeneName _, _, _ -> Error $"Invalid variant names: {variantInput.VariantName}"
                | _ -> Error $"Invalid variant: {variantInput}"

    module Variants =
        open Utilities

        /// Validate a list of variant inputs
        let validate =
            List.map Variant.Input.validate
            >> Result.combine

    module MicrosatelliteStatus =
        type MsInput = MsInput of string

        module Input =
            /// Validate that if a microsatellite input exists: it either cannot be determined, is stable, or has high instability. If not, result in an error.
            let validate (msInput: MsInput option) =
                match msInput with
                | None -> Ok None
                | Some (MsInput "Cannot Be Determined") -> Ok <| Some MicrosatelliteStatus.``Cannot Be Determined``
                | Some (MsInput "MS-Stable") -> Ok <| Some Stable
                | Some (MsInput "MSI-High") -> Ok <| Some ``High Instability``
                | Some _ -> Error $"Invalid MicrosatelliteStatusInput: {msInput}"


    module TumorMutationBurden =
        open FsToolkit.ErrorHandling

        type ScoreInput = ScoreInput of float
        type StatusInput = StatusInput of string

        type Input =
            { ScoreInput: ScoreInput
              StatusInput: StatusInput}

        module Score =
            /// Validate that a tumor mutation burden score is greater than 0.0
            let validate (ScoreInput input) =
                if input >= 0.0 then Ok <| TmbScore (input * 1.0<mutation/megabase>)
                else Error $"Invalid score: {input}"

        module Status =
            /// Validate that a tumor mutation burden status is either low, intermediate, high, or unknown.
            let validate (StatusInput input) =
                match input with
                | "low" -> Ok Low
                | "intermediate" -> Ok Intermediate
                | "high" -> Ok High
                | "unknown" -> Ok UnknownStatus
                | _ -> Error $"Invalid tmb status: {input}"

        let validate tmbInput =
            validation {
                let! score = Score.validate tmbInput.ScoreInput
                and! status = Status.validate tmbInput.StatusInput

                return { Score = score
                         Status = status }
            }

        let validateOptional tmbInput =
            match tmbInput with
            | Some tmbI -> validate tmbI |> Result.map Some
            | None -> Ok None

    module Lab =
        module Address =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate a FMI lab address is valid
            let validate (Input input) =
                let regex = Regex("(?<street_address>(.)+), (?<city>[a-zA-Z]+), (?<state>[a-zA-Z]{2}) (?<zip_code>\d+)$").Match(input)

                if regex.Success then
                    Ok <| { StreetAddress = (StreetAddress regex.Groups.["streetAddress"].Value)
                            City = City regex.Groups.["city"].Value
                            State = State regex.Groups.["state"].Value
                            Zipcode = Zipcode regex.Groups.["zip_code"].Value }
                else Error $"Invalid lab address: {input}"

        type Input =
            { AddressInput: Address.Input
              CliaNumber: CliaNumber}

        open FsToolkit.ErrorHandling

        /// Validate the report's lab address and clia number
        let validate input =
            validation {
                let! address = Address.validate input.AddressInput

                return { Address = address
                         CliaNumber = input.CliaNumber }
            }

    module Report =
        open FSharp.Data
        open System.IO
        open System.Xml.Linq

        type Input =
            { ReportIdInput: ReportId.Input
              IssuedDateInput: IssuedDate.Input
              LabInput: Lab.Input
              SampleInput: Sample.Input
              PmiInput: PMI.Input
              MsStatusInput: MicrosatelliteStatus.MsInput option
              TmbInput: TumorMutationBurden.Input option
              VariantsInput: Variant.Input list }

        [<Literal>]
        let ClinicalReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/clinicalReport.xsd"
        [<Literal>]
        let VariantReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/variantReport.xsd"

        type ClinicalReportProvider = XmlProvider<Schema=ClinicalReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.clinicalReport.xsd">
        type VariantReportProvider = XmlProvider<Schema=VariantReportXsdPath, EmbeddedResource=" OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.variantReport.xsd">

        type Xml(filePath: string) =
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

            /// Retrieve the report's microsatellite status, if it exists.
            member this.MicrosatelliteStatus =
                this.ClinicalReport.Genes
                |> Seq.tryFind (fun gene -> gene.Name = "Microsatellite status")
                |> Option.map (fun msStatus -> Seq.head(msStatus.Alterations).Name)

            member this.Biomarkers =
                this.VariantReport.Biomarkers

            member this.TumorMutationBurden =
                this.Biomarkers.TumorMutationBurden
                |> Option.map (fun tmb -> (tmb.Score, tmb.Status))


            (* Read in XML to report input *)

            member this.ReportIdInput = ReportId.Input this.ClinicalReport.ReportId

            member this.IssuedDateInput =
                this.ServerTime |> IssuedDate.Input

            /// Retrieve the lab's address and clia number
            member this.LabInput: Lab.Input =
                let processSite = this.ReportSample.ProcessSites.[0]

                { AddressInput = Lab.Address.Input processSite.Address
                  CliaNumber = CliaNumber processSite.CliaNumber }

            /// Retrieve the report's sample
            member this.SampleInput : Sample.Input =
                { SampleIdInput = Sample.SampleId.Input this.ReportSample.SampleId
                  BlockId = Sample.BlockId.Input this.ReportSample.BlockId
                  ReceivedDate = ReceivedDate this.ReportSample.ReceivedDate
                  SampleFormat = Sample.SampleFormat.Input this.ReportSample.SpecFormat }

            /// Retrieve the report's patient medical information
            member this.PmiInput : PMI.Input =
                let pmi = this.ClinicalReport.Pmi

                { MrnInput = MRN.Input pmi.Mrn
                  LastName = LastName pmi.LastName
                  FirstName = FirstName pmi.FirstName
                  SubmittedDiagnosis = SubmittedDiagnosis pmi.SubmittedDiagnosis
                  GenderInput = PMI.Gender.Input pmi.Gender
                  DateOfBirth = DateOfBirth pmi.Dob
                  SpecimenSite = SpecimenSite pmi.SpecSite
                  CollectionDate = CollectionDate pmi.CollDate
                  OrderingMd = { MdName = OrderingMdName pmi.OrderingMd; MdId = OrderingMdId pmi.OrderingMdId }
                  Pathologist = Pathologist pmi.Pathologist }

            /// Retrieve the report's variants, including gene name, VUS status, and variant name(s)
            member this.VariantInputs : Variant.Input seq =
                this.ClinicalReport.VariantProperties
                |> Seq.map (fun variantProperty ->
                    { GeneName = GeneName variantProperty.GeneName
                      IsVus = Variant.IsVus variantProperty.IsVus
                      VariantName = VariantName variantProperty.VariantName })

            member this.MicrosatelliteStatusInput =
                this.MicrosatelliteStatus
                |> Option.map MicrosatelliteStatus.MsInput

            member this.TmbInput : TumorMutationBurden.Input option =
                this.TumorMutationBurden
                |> Option.map (fun (score, status) ->
                    { ScoreInput = TumorMutationBurden.ScoreInput (float score)
                      StatusInput = TumorMutationBurden.StatusInput status })

            member this.ReportInput =
                { ReportIdInput = this.ReportIdInput
                  IssuedDateInput = this.IssuedDateInput
                  LabInput = this.LabInput
                  SampleInput = this.SampleInput
                  PmiInput = this.PmiInput
                  MsStatusInput = this.MicrosatelliteStatusInput
                  TmbInput = this.TmbInput
                  VariantsInput = this.VariantInputs |> Seq.toList }


        open FsToolkit.ErrorHandling

        let validate input =
            validation {
                let! reportId = ReportId.validate input.ReportIdInput
                and! sample = Sample.validate input.SampleInput
                and! pmi = PMI.validate input.PmiInput
                and! lab = Lab.validate input.LabInput
                and! msStatus = MicrosatelliteStatus.Input.validate input.MsStatusInput
                and! tmb = TumorMutationBurden.validateOptional input.TmbInput
                and! variants = Variants.validate input.VariantsInput
                and! issuedDate = IssuedDate.validate input.IssuedDateInput

                return { ReportId = reportId
                         Sample = sample
                         PMI = pmi
                         IssuedDate = issuedDate
                         MicrosatelliteStatus = msStatus
                         TumorMutationBurden = tmb
                         Lab = lab
                         Variants = variants }
            }