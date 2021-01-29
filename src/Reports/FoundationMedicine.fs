namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    type ReportId = ReportId of string

    type Unvalidated = Unvalidated
    type Invalid = internal | Invalid
    type Valid = internal | Valid

    type Sample<'Status> =
        { SampleId: SampleId<'Status>
          SampleName: SampleName<'Status>
          ReceivedDate: ReceivedDate<'Status>
          BlockId: BlockId<'Status>
          SpecimenFormat: SpecimenFormat<'Status> }
    and SampleId<'Status> = SampleId of string
    and SampleName<'Status> = SampleName of string
    and ReceivedDate<'Status> = ReceivedDate of System.DateTime
    and BlockId<'Status> = BlockId of string
    and SpecimenFormat<'Status> = SpecimenFormat of string

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

            module SpecFormat =

                /// Validate that a sample's specimen format is not blank.
                let validate ((SpecimenFormat specFormat): SpecimenFormat<Unvalidated>) : Result<SpecimenFormat<Valid>,string> =
                    if not (specFormat = "") then
                        Ok <| SpecimenFormat specFormat
                    else
                        Error $"Specimen format cannot be blank"





    module Report =
        open FSharp.Data
        open System.IO
        open System.Xml.Linq

        type ClinicalReportProvider = XmlProvider<Schema="./data/FMI/clinicalReport.xsd", EmbeddedResource="Report, clinicalReport.xsd">
        type VariantReportProvider = XmlProvider<Schema="./data/FMI/variantReport.xsd", EmbeddedResource="Report, variantReport.xsd">

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
                  SampleName = SampleName this.ClinicalReport.SampleName
                  BlockId = BlockId this.ClinicalReport.Sample.BlockId
                  ReceivedDate = ReceivedDate this.ClinicalReport.Sample.ReceivedDate
                  SpecimenFormat = SpecimenFormat this.ClinicalReport.Sample.SpecFormat }

//            member this.PMI =
//                let reportPmi = this.ClinicalReport.Pmi
//                {| MRN = MRN reportPmi.Mrn
//                   LastName = LastName reportPmi.LastName
//                   FirstName = FirstName reportPmi.FirstName
//                   SubmittedDiagnosis = SubmittedDiagnosis reportPmi.SubmittedDiagnosis
//                   Gender = reportPmi.Gender
//                   DateOfBirth = DateOfBirth reportPmi.Dob
//                   OrderingMd = { MdName = OrderingMdName reportPmi.OrderingMd; MdId = OrderingMdId reportPmi.OrderingMdId }
//                   SpecimenSite = reportPmi.SpecSite
//                   CollectionDate = reportPmi.CollDate
//                   ReceivedDate = reportPmi.ReceivedDate |}
