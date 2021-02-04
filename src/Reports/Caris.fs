namespace OSTOR.ClinicalTrials.Reports

module Caris =
    module Report =
        open FSharp.Data
        open System.IO

        [<Literal>]
        let CarisReportXsdPath = __SOURCE_DIRECTORY__ + "/data/carisReport.xsd"

        type ReportProvider = XmlProvider<Schema=CarisReportXsdPath, EmbeddedResource="Reports, carisReport.xsd">

        type Xml(filePath: string) =
            let filePath = filePath
            let text = File.ReadAllText(filePath)
            let report = ReportProvider.Parse(text)

            let testDetails = report.TestDetails
            let patientInfo = report.PatientInformation

            member _.TumorSpecimenInfo = report.SpecimenInformation.TumorSpecimenInformation
            member _.OrderedDate = testDetails.OrderedDate
            member _.ReceivedDate = testDetails.ReceivedDate
            member _.ReportId = testDetails.LabReportId

            member _.Patient =
                {| LastName = patientInfo.LastName
                   FirstName = patientInfo.FirstName
                   DateOfBirth = patientInfo.Dob
                   Gender = patientInfo.Gender |}