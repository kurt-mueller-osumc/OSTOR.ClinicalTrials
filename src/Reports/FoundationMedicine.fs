namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    [<Measure>] type mutation
    [<Measure>] type megabase

    type ReportId = internal ReportId of string

    type Sample =
        { SampleId: SampleId
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          SpecimenFormat: SpecimenFormat }
    and SampleId = internal | SampleId of string
    and ReceivedDate = internal | ReceivedDate of System.DateTime
    and BlockId = internal | BlockId of string
    and SpecimenFormat = internal | SpecimenFormat of string

    type PMI =
        { MRN: MRN
          LastName: LastName
          FirstName: FirstName
          SubmittedDiagnosis: SubmittedDiagnosis
          Gender: Gender
          DateOfBirth: DateOfBirth
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate
          OrderingMd: OrderingMd }
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

    type TumorMutationalBurden =
        internal
        | TumorMutationalBurden of rate : float<mutation/megabase>
        | ``Cannot Be Determined``

    type Report =
        { MicrosatelliteStatus: MicrosatelliteStatus option }

    module ReportId =
        open System.Text.RegularExpressions

        type Input = Input of string

        /// Validate that a sample id is the following format where 'd' is a digit: ORD-ddddddd-dd
        ///
        ///    validate (ReportId "ORD-1234567-89") = Ok (ReportId "ORD-1234567-89")
        ///    validate (ReportId "invalidId") = Error "Invalid report id: invalidId"
        let validate (Input reportId) =
            if Regex("ORD-\d{7,}-\d{2,}").Match(reportId).Success then
                Ok <| ReportId reportId
            else
                Error <| $"Invalid report id: {reportId}"

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

        module SpecimenFormat =
            type Input = Input of string

            /// Validate that a sample's specimen format is not blank.
            let validate (Input input) =
                if input <> "" then
                    Ok <| SpecimenFormat input
                else
                    Error $"Specimen format cannot be blank"

        type Input =
            { SampleIdInput: SampleId.Input
              ReceivedDate: ReceivedDate
              BlockId: BlockId.Input
              SpecimenFormat: SpecimenFormat.Input }

        /// Validate a sample from a FMI report
        let validate (sampleInput: Input) =
            validation {
                let! sampleId = SampleId.validate sampleInput.SampleIdInput
                and! blockId = BlockId.validate sampleInput.BlockId
                and! specimenFormat = SpecimenFormat.validate sampleInput.SpecimenFormat

                return { SampleId = sampleId
                         ReceivedDate = sampleInput.ReceivedDate
                         BlockId = blockId
                         SpecimenFormat = specimenFormat }
            }

    module PMI =
        open FsToolkit.ErrorHandling

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
              OrderingMd: OrderingMd }

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
                         OrderingMd = pmiInput.OrderingMd }
            }

    module Variant =
        type Input =
            { GeneName: GeneName
              IsVus: IsVUS
              VariantName: VariantName }
        and IsVUS = IsVUS of bool

        module Input =
            open Utilities

            let (|InvalidVariantNames|ValidVariantNames|) (VariantName variantName) =
                if variantName <> "" then
                    let variantNames =
                        variantName
                        |> String.split ','
                        |> List.map VariantName

                    ValidVariantNames variantNames
                else InvalidVariantNames

            let (|InvalidGeneName|ValidGeneName|) (GeneName geneName) =
                if geneName <> "" then ValidGeneName (GeneName geneName)
                else InvalidGeneName

            let (|IsVus|NotVus|) (IsVUS isVus) =
                match isVus with
                | true -> IsVus
                | false -> NotVus

            /// Validate that a variant input has a gene name and at least one variant name.
            let validate (variantInput: Input) =
                match (variantInput.GeneName, variantInput.IsVus, variantInput.VariantName) with
                | ValidGeneName geneName, IsVus, ValidVariantNames variantNames -> Ok <| VariantOfUnknownSignificance { GeneName = geneName; VariantNames = variantNames }
                | ValidGeneName geneName, NotVus, ValidVariantNames variantNames -> Ok <| VariantOfKnownSignificance { GeneName = geneName; VariantNames = variantNames }
                | InvalidGeneName, _, ValidVariantNames _ -> Error $"Invalid gene name: {variantInput.GeneName}"
                | ValidGeneName _, _, InvalidVariantNames -> Error $"Invalid variant names: {variantInput.VariantName}"
                | _ -> Error $"Invalid variant: {variantInput}"

    module MicrosatelliteStatus =
        type Input = Input of string

        module Input =
            let (|MsiHigh|MsStable|CannotBeDetermined|InvalidMsStatus|) (Input msInput) =
                match msInput with
                | "Cannot Be Determined" -> CannotBeDetermined
                | "MS-Stable" -> MsStable
                | "MSI-High" -> MsiHigh
                | _ -> InvalidMsStatus

            /// Validate that if a microsatellite input exists, it either cannot be determined, is stable, or has high instability. If not, result in an error.
            let validate (msInput: Input option) =
                match msInput with
                | None -> Ok None
                | Some CannotBeDetermined -> Ok <| Some MicrosatelliteStatus.``Cannot Be Determined``
                | Some MsStable -> Ok <| Some Stable
                | Some MsiHigh -> Ok <| Some ``High Instability``
                | Some _ -> Error $"Invalid MsStatusInput: {msInput}"

    module Report =
        open FSharp.Data
        open System.IO
        open System.Xml.Linq

        [<Literal>]
        let ClinicalReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/clinicalReport.xsd"
        [<Literal>]
        let VariantReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/variantReport.xsd"

        type ClinicalReportProvider = XmlProvider<Schema=ClinicalReportXsdPath, EmbeddedResource="Reports, clinicalReport.xsd">
        type VariantReportProvider = XmlProvider<Schema=VariantReportXsdPath, EmbeddedResource="Reports, variantReport.xsd">

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

            member this.ReportId = ReportId this.ClinicalReport.ReportId

            member this.ReportSample = this.ClinicalReport.Sample

            /// Retrieve the report's sample
            member this.SampleInput : Sample.Input =
                { SampleIdInput = Sample.SampleId.Input this.ClinicalReport.Sample.SampleId
                  BlockId = Sample.BlockId.Input this.ClinicalReport.Sample.BlockId
                  ReceivedDate = ReceivedDate this.ClinicalReport.Sample.ReceivedDate
                  SpecimenFormat = Sample.SpecimenFormat.Input this.ClinicalReport.Sample.SpecFormat }

            /// Retrieve the report's patient medical information
            member this.PMI : PMI.Input =
                let pmi = this.ClinicalReport.Pmi

                { MrnInput = MRN.Input pmi.Mrn
                  LastName = LastName pmi.LastName
                  FirstName = FirstName pmi.FirstName
                  SubmittedDiagnosis = SubmittedDiagnosis pmi.SubmittedDiagnosis
                  GenderInput = PMI.Gender.Input pmi.Gender
                  DateOfBirth = DateOfBirth pmi.Dob
                  SpecimenSite = SpecimenSite pmi.SpecSite
                  CollectionDate = CollectionDate pmi.CollDate
                  OrderingMd = { MdName = OrderingMdName pmi.OrderingMd; MdId = OrderingMdId pmi.OrderingMdId } }

            /// Retrieve the report's variants, including gene name, VUS status, and variant name(s)
            member this.VariantInputs : Variant.Input seq =
                this.ClinicalReport.VariantProperties
                |> Seq.map (fun variantProperty ->
                    { GeneName =  GeneName variantProperty.GeneName
                      IsVus = Variant.IsVUS variantProperty.IsVus
                      VariantName = VariantName variantProperty.VariantName })

            /// Retrieve the report's microsatellite status, if it exists.
            member this.MicrosatelliteStatus =
                this.ClinicalReport.Genes
                |> Seq.tryFind (fun gene -> gene.Name = "Microsatellite status")
                |> Option.map (fun msStatus -> Seq.head(msStatus.Alterations).Name)
                |> Option.map MicrosatelliteStatus.Input

            member this.TumorMutationalBurden =
                this.ClinicalReport.Genes
                |> Seq.tryFind (fun gene -> gene.Name = "Tumor Mutation Burden")
                |> Option.map (fun tmb -> Seq.head(tmb.Alterations).Name)