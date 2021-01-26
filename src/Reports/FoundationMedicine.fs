namespace OSTOR.ClinicalTrials.Reports

module FoundationMedicine =
    type ReportId = ReportId of string

    type Sample =
        { ReportId: ReportId // FM_Id
          SampleId: SampleId
          ReceivedDate: ReceivedDate
          BlockId: BlockId }
    and SampleId = SampleId of string
    and ReceivedDate = ReceivedDate of System.DateTime
    and BlockId = BlockId of string

    type SampleInput =
        { ReportId: ReportId
          ReceivedDateInput: ReceivedDateInput }
    and ReceivedDateInput = ReceivedDateInput of string
