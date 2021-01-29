namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    type ReportId = internal ReportId of string
    type Unvalidated = Unvalidated
    type Invalid = internal | Invalid
    type Valid = internal | Valid

    type Sample<'Status> =
        { SampleId: SampleId<'Status>
          ReceivedDate: ReceivedDate<'Status>
          BlockId: BlockId<'Status>
          SpecimenFormat: SpecimenFormat<'Status> }
    and SampleId<'Status> = internal | SampleId of string
    and ReceivedDate<'Status> = internal | ReceivedDate of System.DateTime
    and BlockId<'Status> = internal | BlockId of string
    and SpecimenFormat<'Status> = internal | SpecimenFormat of string

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
    and MRN = MRN of string
    and LastName = LastName of string
    and FirstName = FirstName of string
    and SubmittedDiagnosis = SubmittedDiagnosis of string
    and Gender = Male | Female
    and DateOfBirth = DateOfBirth of System.DateTime
    and SpecimenSite = SpecimenSite of string
    and CollectionDate = CollectionDate of System.DateTime
    and OrderingMd =
        { MdName: OrderingMdName
          MdId: OrderingMdId }
    and OrderingMdName = OrderingMdName of string
    and OrderingMdId = OrderingMdId of string

    type PmiInput =
      { ReportId: ReportId
        MrnInput: MrnInput
        LastName: LastName
        FirstName: FirstName
        SubmittedDiagnosis: SubmittedDiagnosis }
    and MrnInput = MrnInput of string

    type Variant =
        | VariantOfUnknownSignificance of VariantProperty
        | VariantOfKnownSignificance of VariantProperty
    and VariantProperty =
        { GeneName: GeneName
          VariantName: VariantName }
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

    module Validation =
        open FsToolkit.ErrorHandling

        module Sample =
            module SampleId =
                open System.Text.RegularExpressions

                /// Validate that a sample id is the following format where 'd' is a digit: ORD-ddddddd-dd
                ///
                ///    validate (SampleId "ORD-1234567-89") = Ok (SampleId "ORD-1234567-89)
                ///    validate (SampleId "invalidId") = Error "Sample id not valid: invalidId"
                let validate ((SampleId sampleId): SampleId<Unvalidated>) : Result<SampleId<Valid>,string> =
                    if Regex("ORD-\d{7,}-\d{2,}").Match(sampleId).Success then
                        Ok <| SampleId sampleId
                    else
                        Error <| $"Sample id not valid: {sampleId}"

            module ReceivedDate =
                let validate ((ReceivedDate receivedDate): ReceivedDate<Unvalidated>) : Result<ReceivedDate<Valid>, string> =
                    Ok (ReceivedDate receivedDate)

            module BlockId =
                open System.Text.RegularExpressions

                /// Validate that a sample's block id is in the following format where 'd' is a digit: USddddddd.dd
                ///
                ///    validate (BlockId US0123456.78) = Ok (BlockId "US0123456.78")
                ///    validate (BlockId "invalidId") = Error "Block id is not valid: invalidId"
                let validate ((BlockId blockId): BlockId<Unvalidated>) : Result<BlockId<Valid>,string> =
                    if Regex("US\d+.\d+").Match(blockId).Success then
                        Ok <| BlockId blockId
                    else
                        Error $"Block id not valid: {blockId}"

            module SpecimenFormat =

                /// Validate that a sample's specimen format is not blank.
                let validate ((SpecimenFormat specFormat): SpecimenFormat<Unvalidated>) : Result<SpecimenFormat<Valid>,string> =
                    if not (specFormat = "") then
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

    module Report =
        open FSharp.Data
        open System.IO
        open System.Xml.Linq

        [<Literal>]
        let ClinicalReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/clinicalReport.xsd"
        [<Literal>]
        let VariantReportXsdPath = __SOURCE_DIRECTORY__ + "/data/FMI/variantReport.xsd"

        type ClinicalReportProvider = XmlProvider<Schema= ClinicalReportXsdPath, EmbeddedResource="Reports, clinicalReport.xsd">
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

            member this.Sample : Sample<Unvalidated> =
                { SampleId = SampleId this.ClinicalReport.Sample.SampleId
                  BlockId = BlockId this.ClinicalReport.Sample.BlockId
                  ReceivedDate = ReceivedDate this.ClinicalReport.Sample.ReceivedDate
                  SpecimenFormat = SpecimenFormat this.ClinicalReport.Sample.SpecFormat }
