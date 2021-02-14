namespace OSTOR.ClinicalTrials.Reports

module Caris =
    open Common

    type Patient =
        { MRN: MRN
          LastName: LastName
          FirstName: FirstName
          DateOfBirth: System.DateTime
          Sex: Sex }
    and LastName = internal LastName of string
    and FirstName = internal FirstName of string
    and Sex = internal Male | Female

    type Specimen =
        { SpecimenId: SpecimenId
          SpecimenType: SpecimenType
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate }
    and SpecimenId = SpecimenId of string
    and SpecimenType =
        internal
        | ``Tissue Biopsy Formalin Vial``
        | ``Tissue Biopsy Paraffin Blocks``
        | ``Tissue Biopsy Slide Unstained``
    and SpecimenSite = SpecimenSite of string
    and CollectionDate = CollectionDate of System.DateTime
    and ReceivedDate = ReceivedDate of System.DateTime

    module Patient =
        open FsToolkit.ErrorHandling

        module LastName =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map LastName
                |> Result.mapError (fun e -> $"LastName: {e}")

        module FirstName =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map FirstName
                |> Result.mapError (fun e -> $"FirstName: {e}")

        module Sex =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "Male" | "male" -> Ok Male
                | "Female" | "female" -> Ok Female
                | _ -> Error $"Sex: Not a valid sex ({input})"

        type Input =
            { MrnInput: MRN.Input
              LastName: LastName.Input
              FirstName: FirstName.Input
              DateOfBirth: System.DateTime
              Sex: Sex.Input }

        /// Validate a report's patient information
        let validate input =
            validation {
                let! mrn = MRN.validate input.MrnInput
                and! lastName = LastName.validate input.LastName
                and! firstName = FirstName.validate input.FirstName
                and! sex = Sex.validate input.Sex

                return { MRN = mrn
                         LastName = lastName
                         FirstName = firstName
                         Sex = sex
                         DateOfBirth = input.DateOfBirth }
            }


    type Report =
        { MRN: MRN option
          Specimen: Specimen }

    module Report =
        open FSharp.Data
        open System.IO
        open Utilities

        [<Literal>]
        let CarisReportXsdPath = __SOURCE_DIRECTORY__ + "/data/carisReport.xsd"

        type ReportProvider = XmlProvider<Schema=CarisReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.carisReport.xsd">

        type Xml(filePath: string) =
            let filePath = filePath
            let text = File.ReadAllText(filePath)
            let report = ReportProvider.Parse(text)

            let testDetails = report.TestDetails
            let patientInfo = report.PatientInformation
            let tests = report.Tests
            let testResults = tests |> Seq.collect (fun test -> test.TestResults)

            member _.TumorSpecimenInfo = report.SpecimenInformation.TumorSpecimenInformation
            member _.OrderedDate = testDetails.OrderedDate
            member _.ReceivedDate = testDetails.ReceivedDate
            member _.ReportId = testDetails.LabReportId

            member _.GenomicAlterations =
                testResults
                |> Seq.map (fun testResult -> testResult.GenomicAlteration)
                |> Seq.choose id

            member _.ExpressionAlterations =
                testResults
                |> Seq.map (fun testResult -> testResult.ExpressionAlteration)
                |> Seq.choose id

            /// Expression alterations where the test result is 'Positive'
            member this.PositiveExpressionAlterations =
                this.ExpressionAlterations
                |> Seq.filter (fun expressionAlteration -> expressionAlteration.Result = "Positive")

            // member this.GenomicAlterationInputs =
            //     this.GenomicAlterations
            //     |> Seq.map (fun genomicAlteration ->
            //         genomicAlteration.
            //     )


            member _.Patient =
                {| LastName = patientInfo.LastName
                   FirstName = patientInfo.FirstName
                   DateOfBirth = patientInfo.Dob
                   Gender = patientInfo.Gender
                   MRN = patientInfo.Mrn |}