namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    [<AutoOpen>]
    module Measures =
        [<Measure>] type mutation
        [<Measure>] type megabase

    type Unvalidated = Unvalidated
    type Invalid = internal | Invalid
    type Valid = internal | Valid

    type ReportId<'Status> = internal ReportId of string

    type Sample<'Validation> =
        { SampleId: SampleId<'Validation>
          ReceivedDate: ReceivedDate<'Validation>
          BlockId: BlockId<'Validation>
          SpecimenFormat: SpecimenFormat<'Validation> }
    and SampleId<'Validation> = internal | SampleId of string
    and ReceivedDate<'Validation> = internal | ReceivedDate of System.DateTime
    and BlockId<'Validation> = internal | BlockId of string
    and SpecimenFormat<'Validation> = internal | SpecimenFormat of string

    type PMI<'Validation> =
        { MRN: MRN<'Validation>
          LastName: LastName
          FirstName: FirstName
          SubmittedDiagnosis: SubmittedDiagnosis
          Gender: Gender<'Validation>
          DateOfBirth: DateOfBirth
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate
          OrderingMd: OrderingMd }
    and MRN<'Validation> = MRN of string
    and LastName = LastName of string
    and FirstName = FirstName of string
    and SubmittedDiagnosis = SubmittedDiagnosis of string
    and Gender<'Validation> = Gender of string
    and DateOfBirth = DateOfBirth of System.DateTime
    and SpecimenSite = SpecimenSite of string
    and CollectionDate = CollectionDate of System.DateTime
    and OrderingMd =
        { MdName: OrderingMdName
          MdId: OrderingMdId }
    and OrderingMdName = OrderingMdName of string
    and OrderingMdId = OrderingMdId of string

    type Variant =
        | VariantOfUnknownSignificance of VariantInfo
        | VariantOfKnownSignificance of VariantInfo
    and VariantInfo =
        { GeneName: GeneName
          VariantNames: VariantName list }
    and GeneName = GeneName of string
    and VariantName = VariantName of string

    type Gene =
        { GeneName: GeneName
          GeneAlterations: GeneAlteration list }
    and GeneAlteration =
        { AlterationName: GeneAlterationName
          Interpretation: GeneAlterationInterpretation }
    and GeneAlterationName = GeneAlterationName of name: string
    and GeneAlterationInterpretation = GeneAlterationInterpretation of interpretation: string

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

    module Validation =
        open FsToolkit.ErrorHandling

        module ReportId =
            open System.Text.RegularExpressions

            /// Validate that a sample id is the following format where 'd' is a digit: ORD-ddddddd-dd
            ///
            ///    validate (ReportId "ORD-1234567-89") = Ok (ReportId "ORD-1234567-89")
            ///    validate (ReportId "invalidId") = Error "Invalid report id: invalidId"
            let validate ((ReportId reportId): ReportId<Unvalidated>) : Result<ReportId<Valid>,string> =
                if Regex("ORD-\d{7,}-\d{2,}").Match(reportId).Success then
                    Ok <| ReportId reportId
                else
                    Error <| $"Invalid report id: {reportId}"

        module Sample =
            module SampleId =
                open System.Text.RegularExpressions

                /// Validate that a sample's id is in the following format where 'd' is a digit: USddddddd.dd
                ///
                ///    validate (SampleId "US0123456.78") = Ok (SampleId "US0123456.78")
                ///    validate (SampleId "invalidId") = Error "Invalid sample id: invalidId"
                let validate ((SampleId sampleId): SampleId<Unvalidated>) : Result<SampleId<Valid>,string> =
                    if Regex("US\d{7,}.\d{2,}").Match(sampleId).Success then
                        Ok <| SampleId sampleId
                    else
                        Error <| $"Invalid sample id: {sampleId}"

            module ReceivedDate =
                let validate ((ReceivedDate receivedDate): ReceivedDate<Unvalidated>) : Result<ReceivedDate<Valid>, string> =
                    Ok (ReceivedDate receivedDate)

            module BlockId =
                /// Validate that a block id is not blank.
                let validate ((BlockId blockId): BlockId<Unvalidated>) : Result<BlockId<Valid>,string> =
                    if blockId <> "" then
                        Ok <| BlockId blockId
                    else
                        Error $"Invalid block id: {blockId}"

            module SpecimenFormat =
                /// Validate that a sample's specimen format is not blank.
                let validate ((SpecimenFormat specFormat): SpecimenFormat<Unvalidated>) : Result<SpecimenFormat<Valid>,string> =
                    if specFormat <> "" then
                        Ok <| SpecimenFormat specFormat
                    else
                        Error $"Specimen format cannot be blank"

            /// Validate a sample from a FMI report
            let validate (sample: Sample<Unvalidated>) =
                validation {
                    let! sampleId = SampleId.validate sample.SampleId
                    and! receivedDate = ReceivedDate.validate sample.ReceivedDate
                    and! blockId = BlockId.validate sample.BlockId
                    and! specimenFormat = SpecimenFormat.validate sample.SpecimenFormat

                    return { SampleId = sampleId
                             ReceivedDate = receivedDate
                             BlockId = blockId
                             SpecimenFormat = specimenFormat }
                }

    module PMI =
        open FsToolkit.ErrorHandling
        open System.Text.RegularExpressions

        module MRN =
            let validate ((MRN mrn): MRN<Unvalidated>) : Result<MRN<Valid>,string> =
                if Regex("^(\d|[a-z]|[A-Z])+$").Match(mrn).Success then
                    Ok (MRN mrn)
                else
                    Error $"Invalid MRN: {mrn}"

        module Gender =
            let validate ((Gender gender): Gender<Unvalidated>) : Result<Gender<Valid>,string> =
                match gender with
                | "male" | "female" -> Ok (Gender gender)
                | _ -> Error $"Invalid gender: {gender}"

        let validate (pmi: PMI<Unvalidated>) =
            validation {
                let! mrn = MRN.validate pmi.MRN
                and! gender = Gender.validate pmi.Gender

                return { MRN = mrn
                         Gender = gender
                         LastName = pmi.LastName
                         FirstName = pmi.FirstName
                         SubmittedDiagnosis = pmi.SubmittedDiagnosis
                         DateOfBirth = pmi.DateOfBirth
                         SpecimenSite = pmi.SpecimenSite
                         CollectionDate = pmi.CollectionDate
                         OrderingMd = pmi.OrderingMd }
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
            member this.Sample : Sample<Unvalidated> =
                { SampleId = SampleId this.ClinicalReport.Sample.SampleId
                  BlockId = BlockId this.ClinicalReport.Sample.BlockId
                  ReceivedDate = ReceivedDate this.ClinicalReport.Sample.ReceivedDate
                  SpecimenFormat = SpecimenFormat this.ClinicalReport.Sample.SpecFormat }

            /// Retrieve the report's patient medical information
            member this.PMI : PMI<Unvalidated> =
                let pmi = this.ClinicalReport.Pmi

                { MRN = MRN pmi.Mrn
                  LastName = LastName pmi.LastName
                  FirstName = FirstName pmi.FirstName
                  SubmittedDiagnosis = SubmittedDiagnosis pmi.SubmittedDiagnosis
                  Gender = Gender pmi.Gender
                  DateOfBirth = DateOfBirth pmi.Dob
                  SpecimenSite = SpecimenSite pmi.SpecSite
                  CollectionDate = CollectionDate pmi.CollDate
                  OrderingMd = { MdName = OrderingMdName pmi.OrderingMd; MdId = OrderingMdId pmi.OrderingMdId } }

            /// Retrieve the report's variants, including gene name, VUS status, and variant name(s)
            member this.VariantProperties : Variant.Input seq =
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