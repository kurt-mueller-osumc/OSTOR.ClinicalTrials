namespace OSTOR.ClinicalTrials.Reports

module Caris =
    open Common

    type Patient =
        { MRN: MRN
          LastName: LastName
          FirstName: FirstName
          DateOfBirth: DateOfBirth
          Sex: Sex }
    and Sex = internal Male | Female
    and DateOfBirth = internal DateOfBirth of System.DateTime

    type Diagnosis =
        { DiagnosisName: DiagnosisName
          PathologicDiagnosis: PathologicDiagnosis
          DiagnosisSite: DiagnosisSite
          Lineage: Lineage
          SubLineage: SubLineage }
    and DiagnosisName = internal DiagnosisName of string
    and PathologicDiagnosis = internal PathologicDiagnosis of string
    and DiagnosisSite = internal DiagnosisSite of string
    and Lineage = internal Lineage of string
    and SubLineage = internal SubLineage of string

    type OrderingMd =
        { OrderingMdName: FullName
          NationalProviderId: NationalProviderId }

    type Specimen =
        { SpecimenId: SpecimenId
          AccessionId: AccessionId
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
    and AccessionId = AccessionId of string

    type Test =
        { OrderedDate: OrderedDate
          ReceivedDate: ReceivedDate
          ReportId: ReportId }
    and OrderedDate = internal OrderedDate of System.DateTime
    and ReportId = ReportId of string

    type GenomicAlteration =
        { ResultGroup: GenomicAlterationResultGroup }

    /// Genomic alterations will be grouped as either:
    /// - mutated
    /// - no result
    /// - normal
    and GenomicAlterationResultGroup =
        internal
        | Mutated of MutatedResults
        | NoResult of NoResults
        | Normal of NormalResults option

    /// Valid results for mutated genomic alterations
    and MutatedResults =
        internal
        | ``Likely Pathogenic Variant``
        | ``Mutated - Other``
        | ``Mutated, Pathogenic``
        | ``Mutated, Presumed Benign``
        | ``Mutated, Presumed Pathogenic``
        | ``Mutated, Variant of Unknown Significance``
        | Pathogenic
        | ``Pathogenic Variant``
        | ``Presumed Benign``
        | ``Presumed Pathogenic``
        | ``Variant of Unknown Significance``

    /// Valid results for genomic alteration that didn't have a result
    and NoResults =
        internal
        | Indeterminate
        | ``Likely Benign Variant``
        | ``Variant of Uncertain Significance``
        | ``Variant Not Detected``

    /// Valid results for normal genomic alterations
    and NormalResults =
        internal
        | ``Mutation Not Detected``
        | ``Wild Type``


    module Patient =
        open FsToolkit.ErrorHandling

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
                         DateOfBirth = DateOfBirth input.DateOfBirth }
            }

    module OrderingMd =
        open FsToolkit.ErrorHandling

        type Input =
            { NameInput: FullName.Input
              NationalProviderIdInput: NationalProviderId.Input }

        /// Validate the presence an ordering md's name and the format of their national provider id
        let validate input =
            validation {
                let! name = FullName.validate input.NameInput
                and! npi = NationalProviderId.validate input.NationalProviderIdInput

                return { OrderingMdName = name; NationalProviderId = npi }
            }

    module Test =
        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        module OrderedDate =
            type Input = Input of string

            /// Validate the ordered date for a test
            let validate (Input input) =
                input
                |> validateDateTime
                |> Result.map OrderedDate
                |> Result.mapError (fun e -> $"OrderedDate - {e}")

        module ReceivedDate =
            type Input = Input of string

            /// Validate the received date for a test
            let validate (Input input) =
                input
                |> validateDateTime
                |> Result.map ReceivedDate
                |> Result.mapError (fun e -> $"ReceivedDate - {e}")

        module ReportId =
            open System.Text.RegularExpressions

            type Input = Input of string

            let validate (Input input) =
                if Regex("^TN\d{2}-\d{6}$").Match(input).Success then
                    Ok <| ReportId input
                else Error $"ReportId - Invalid report id: {input}"

        type Input =
            { ReportIdInput: ReportId.Input
              OrderedDate: OrderedDate.Input
              ReceivedDate: ReceivedDate.Input }

        /// Validate a test input
        let validate input =
            validation {
                let! reportId = ReportId.validate input.ReportIdInput
                and! orderedDate = OrderedDate.validate input.OrderedDate
                and! receivedDate = ReceivedDate.validate input.ReceivedDate

                return { ReportId = reportId
                         OrderedDate = orderedDate
                         ReceivedDate = receivedDate }
            }

    module GenomicAlteration =
        module BiomarkerName =
            type Input = Input of string

        module GeneName =
            type Input = Input of string

        module ResultGroup =
            let isMutated resultGroup =
                match resultGroup with
                | Mutated _ -> true
                | _ -> false

            let isPathogenic resultGroup =
                match resultGroup with
                | Mutated ``Likely Pathogenic Variant``
                | Mutated ``Mutated, Pathogenic``
                | Mutated Pathogenic
                | Mutated ``Pathogenic Variant``
                | Mutated  ``Presumed Pathogenic`` -> true
                | _ -> false

            let isVariantOfUnknownSignificance resultGroup =
                match resultGroup with
                | Mutated ``Mutated, Variant of Unknown Significance`` -> true
                | _ -> false

            type Input =
                { GroupInput: string
                  ResultInput: string }

            /// Validate that a result group 'mutated', 'no result', or 'normal' nad that the result names are valid.
            let validate input =
                match (input.GroupInput, input.ResultInput) with
                | ("Mutated", "Likely Pathogenic Variant")    -> Ok <| Mutated ``Likely Pathogenic Variant``
                | ("Mutated", "Mutated - Other")              -> Ok <| Mutated ``Mutated - Other``
                | ("Mutated", "Mutated, Pathogenic")          -> Ok <| Mutated ``Mutated, Pathogenic``
                | ("Mutated", "Mutated, Presumed Benign")     -> Ok <| Mutated ``Mutated, Presumed Benign``
                | ("Mutated", "Mutated, Presumed Pathogenic") -> Ok <| Mutated ``Mutated, Presumed Pathogenic``
                | ("Mutated", "Mutated, Variant of Unknown Significance") -> Ok <| Mutated ``Mutated, Variant of Unknown Significance``
                | ("Mutated", "Pathogenic")          -> Ok <| Mutated Pathogenic
                | ("Mutated", "Pathogenic Variant")  -> Ok <| Mutated ``Pathogenic Variant``
                | ("Mutated", "Presumed Benign")     -> Ok <| Mutated ``Presumed Benign``
                | ("Mutated", "Presumed Pathogenic") -> Ok <| Mutated ``Presumed Pathogenic``
                | ("Mutated", "Variant of Unknown Significance") -> Ok <| Mutated ``Variant of Unknown Significance``
                | ("No Result", "Indeterminate")         -> Ok <| NoResult Indeterminate
                | ("No Result", "Likely Benign Variant") -> Ok <| NoResult ``Likely Benign Variant``
                | ("No Result", "Variant of Uncertain Significance") -> Ok <| NoResult ``Variant of Uncertain Significance``
                | ("No Result", "indeterminate")      -> Ok <| NoResult Indeterminate
                | ("No Result", "variantnotdetected") -> Ok <| NoResult ``Variant Not Detected``
                | ("Normal", "") -> Ok <| Normal None
                | ("Normal", "Mutation Not Detected") -> Ok <| Normal (Some ``Mutation Not Detected``)
                | ("Normal", "Wild Type")             -> Ok <| Normal (Some ``Wild Type``)
                | _ -> Error $"Result - Invalid group and name: {input}"

        type Input =
            { BiomarkerName: BiomarkerName.Input
              GeneName: GeneName.Input
              ResultGroup: ResultGroup.Input
              Interpretation: string
              AlleleFrequency: string option }

    type Report =
        { MRN: MRN option
          Specimen: Specimen }

    module Report =
        open FSharp.Data
        open System.IO

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
                |> Seq.map (fun ga ->
                    {| BiomarkerName = ga.BiomarkerNames |> Seq.head
                       GeneName = ga.Genes |> Seq.head
                       Result = ga.Results |> Seq.head
                       ResultGroup = ga.ResultGroups |> Seq.head
                       Interpretation = ga.Interpretations |> Seq.head
                       AlleleFrequency = ga.AlleleFrequencyInformations |> Seq.tryHead |> Option.map (fun afi -> afi.AlleleFrequency) |})

            member _.ExpressionAlterations =
                testResults
                |> Seq.map (fun testResult -> testResult.ExpressionAlteration)
                |> Seq.choose id

            /// Expression alterations where the test result is 'Positive'
            member this.PositiveExpressionAlterations =
                this.ExpressionAlterations
                |> Seq.filter (fun expressionAlteration -> expressionAlteration.Result = "Positive")


            member _.PatientInput : Patient.Input =
                { LastName = LastName.Input patientInfo.LastName
                  FirstName = FirstName.Input patientInfo.FirstName
                  DateOfBirth = patientInfo.Dob
                  Sex = Patient.Sex.Input patientInfo.Gender
                  MrnInput = MRN.Input patientInfo.Mrn }

            member _.Diagnosis =
                { DiagnosisName = DiagnosisName patientInfo.Diagnosis
                  PathologicDiagnosis = PathologicDiagnosis patientInfo.PathologicDiagnosis
                  DiagnosisSite = DiagnosisSite patientInfo.PrimarySite
                  Lineage = Lineage patientInfo.Lineage
                  SubLineage = SubLineage patientInfo.SubLineage }