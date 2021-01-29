namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    type ReportId = ReportId of string

    type Sample =
        { ReportId: ReportId // FM_Id
          SampleId: SampleId
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          SpecimenFormat: SpecimenFormat }
    and SampleId = SampleId of string
    and ReceivedDate = ReceivedDate of System.DateTime
    and BlockId = BlockId of string
    and SpecimenFormat = SpecimenFormat of string

    type SampleInput =
        { ReportId: ReportId
          SampleId: SampleId
          ReceivedDateInput: ReceivedDateInput
          BlockId: BlockId
          SpecimenFormat: SpecimenFormat }
    and ReceivedDateInput = ReceivedDateInput of string

    type PMI =
        { ReportId: ReportId
          MRN: MRN
          LastName: LastName
          FirstName: FirstName
          SubmittedDiagnosis: SubmittedDiagnosis
          Gender: Gender
          DateOfBirth: DateOfBirth
          OrderingMdName: OrderingMdName
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate }
    and MRN = MRN of uint64
    and LastName = LastName of string
    and FirstName = FirstName of string
    and SubmittedDiagnosis = SubmittedDiagnosis of string
    and Gender = Male | Female
    and DateOfBirth = DateOfBirth of System.DateTime
    and OrderingMdName = OrderingMdName of string
    and SpecimenSite = SpecimenSite of string
    and CollectionDate = CollectionDate of System.DateTime

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
