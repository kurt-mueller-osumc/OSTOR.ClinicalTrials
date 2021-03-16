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

    type Diagnosis =
        { DiagnosisName: DiagnosisName
          DiagnosisCode: IcdCode
          PathologicDiagnosis: PathologicDiagnosis
          DiagnosisSite: DiagnosisSite
          Lineage: Lineage
          SubLineage: SubLineage }
    and DiagnosisName = internal DiagnosisName of string
    and PathologicDiagnosis = internal PathologicDiagnosis of string
    and DiagnosisSite = internal DiagnosisSite of string
    and Lineage = internal Lineage of string
    and SubLineage = internal SubLineage of string

    type DiagnosisName with member this.Value = this |> fun (DiagnosisName diagnosisName) -> diagnosisName

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
    and SpecimenId = internal SpecimenId of string
    and SpecimenType =
        internal
        | ``Tissue Biopsy Formalin Vial``
        | ``Tissue Biopsy Paraffin Blocks``
        | ``Tissue Biopsy Slide Unstained``
    and SpecimenSite = internal SpecimenSite of string
    and CollectionDate = internal CollectionDate of System.DateTime
    and ReceivedDate = internal ReceivedDate of System.DateTime
    and AccessionId = internal AccessionId of string

    type Test =
        { LabName: LabName
          OrderedDate: OrderedDate
          ReceivedDate: ReceivedDate
          ReportId: ReportId }
    and LabName     = internal LabName of string
    and OrderedDate = internal OrderedDate of System.DateTime
    and ReportId    = internal ReportId of string

    type LabName     with member this.Value = this |> fun (LabName labName)         -> labName
    type OrderedDate with member this.Value = this |> fun (OrderedDate orderedDate) -> orderedDate
    type ReportId    with member this.Value = this |> fun (ReportId reportId)       -> reportId

    type GenomicAlteration =
        { GeneName: GeneName
          ResultGroup: GenomicAlterationResultGroup
          Source: GenomicAlterationSource option
          Interpretation: GenomicAlterationInterpretation
          AlleleFrequency: AlleleFrequency option }
    and GeneName = internal GeneName of string
    and GenomicAlterationSource = internal | Somatic
    and GenomicAlterationInterpretation = internal GenomicAlterationInterpretation of string
    and AlleleFrequency = internal AlleleFrequency of uint

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


    module Diagnosis =
        module Name =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that diagnosis name is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map DiagnosisName
                |> Result.mapError (fun _ -> "Diagnosis name can't be blank")

        module Site =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that diagnosis site is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map DiagnosisSite
                |> Result.mapError (fun _ -> "Diagnosis site can't be blank")

        module PathologicDiagnosis =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that diagnosis site is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map PathologicDiagnosis
                |> Result.mapError (fun _ -> "Pathologic diagnosis site can't be blank")

        module Lineage =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that lineage is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map Lineage
                |> Result.mapError (fun _ -> "Lineage can't be blank")

        module SubLineage =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that sublineage is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map SubLineage
                |> Result.mapError (fun _ -> "SubLineage can't be blank")

        type Input =
            { DiagnosisCodeInput: IcdCode.Input
              DiagnosisNameInput: Name.Input
              DiagnosisSiteInput: Site.Input
              PathologicDiagnosisInput: PathologicDiagnosis.Input
              LineageInput: Lineage.Input
              SubLineageInput: SubLineage.Input }

        open FsToolkit.ErrorHandling

        let validate (input: Input) =
            validation {
                let! icdCode = input.DiagnosisCodeInput |> IcdCode.validate
                and! diagnosisName = input.DiagnosisNameInput |> Name.validate
                and! diagnosisSite = input.DiagnosisSiteInput |> Site.validate
                and! pathologicDiagnosis = input.PathologicDiagnosisInput |> PathologicDiagnosis.validate
                and! lineage = input.LineageInput |> Lineage.validate
                and! sublineage = input.SubLineageInput |> SubLineage.validate

                return { DiagnosisCode = icdCode
                         DiagnosisName = diagnosisName
                         DiagnosisSite = diagnosisSite
                         PathologicDiagnosis = pathologicDiagnosis
                         Lineage = lineage
                         SubLineage = sublineage }
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

    /// Caris only contains a tumor specimen.
    module TumorSpecimen =
        module SpecimenId =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map SpecimenId
                |> Result.mapError (fun e -> $"Sample id: {e}")

        module AccessionId =
            open System.Text.RegularExpressions

            type Input = Input of string

            let validate (Input input) =
                if Regex("^TN\d{2}-\d{6}-[A-Z]{1}$").Match(input).Success then
                    Ok <| AccessionId input
                else
                    Error $"Invalid accession id: {input}"

        module SpecimenType =
            type Input = Input of string

            /// Validate that a sample type's tissue biopsy is from a vial, blocks, or a slide.
            let validate (Input input) =
                match input with
                | "Tissue Biopsy Formalin Vial" -> Ok ``Tissue Biopsy Formalin Vial``
                | "Tissue Biopsy Paraffin Blocks" -> Ok ``Tissue Biopsy Paraffin Blocks``
                | "Tissue Biopsy Slide Unstained" -> Ok ``Tissue Biopsy Slide Unstained``
                | _ -> Error $"Unknown specimen type: {input}"

        module SpecimenSite =
            open Utilities.StringValidations
            type Input = Input of string

            /// Validate that a specimen site is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map SpecimenSite
                |> Result.mapError (fun e -> $"Specimen site: {e}")

        open FsToolkit.ErrorHandling

        type Input =
            { SpecimenIdInput: SpecimenId.Input
              AccessionIdInput: AccessionId.Input
              SpecimenType: SpecimenType.Input
              SpecimenSite: SpecimenSite.Input
              CollectionDate: CollectionDate
              ReceivedDate: ReceivedDate }

        let validate input =
            validation {
                let! specimenId = SpecimenId.validate input.SpecimenIdInput
                and! accessionId = AccessionId.validate input.AccessionIdInput
                and! specimenType = SpecimenType.validate input.SpecimenType
                and! specimenSite = SpecimenSite.validate input.SpecimenSite

                return { SpecimenId = specimenId
                         AccessionId = accessionId
                         SpecimenType = specimenType
                         SpecimenSite = specimenSite
                         CollectionDate = input.CollectionDate
                         ReceivedDate = input.ReceivedDate }
            }

    module Test =
        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        module LabName =
            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map LabName
                |> Result.mapError (fun _ -> "Lab name can't be blank")

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
            { LabNameInput: LabName.Input
              ReportIdInput: ReportId.Input
              OrderedDate: OrderedDate.Input
              ReceivedDate: ReceivedDate.Input }

        /// Validate a test input
        let validate input =
            validation {
                let! reportId = ReportId.validate input.ReportIdInput
                and! orderedDate = OrderedDate.validate input.OrderedDate
                and! receivedDate = ReceivedDate.validate input.ReceivedDate
                and! labName = LabName.validate input.LabNameInput

                return { ReportId = reportId
                         LabName = labName
                         OrderedDate = orderedDate
                         ReceivedDate = receivedDate }
            }

    module GenomicAlteration =
        module GeneName =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map GeneName
                |> Result.mapError (fun e -> $"Gene name: {e}")

        module Interpretation =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map GenomicAlterationInterpretation
                |> Result.mapError (fun e -> $"Genomic alteration interpretation: {e}")

        module Source =
            type Input = Input of string

            /// Validate that the source of the genomic alteration, if it exists, is somatic.
            let validate (input: Input option) =
                input
                |> Option.map (fun (Input str) ->
                    match str with
                    | "Somatic" -> Ok (Some Somatic)
                    | _ -> Error $"Invalid genomic alteration source: {str}"
                ) |> Option.defaultValue (Ok None)

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

            /// Validate that a result group is 'mutated', 'no result', or 'normal' and that the result names are valid.
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

        module AlleleFrequency =
            open Utilities

            type FrequencyInput = FrequencyInput of string

            module Input =
                let unwrap (FrequencyInput input) = input

                /// Validate that an allele frequency, if it exists, is a positive integer.
                let validate (input : FrequencyInput option) =
                    input
                    |> Option.bind (unwrap >> UnsignedInteger.tryParse)
                    |> Option.map (Ok << (Some << AlleleFrequency))
                    |> Option.defaultValue (Ok None)
                    |> Result.mapError (fun e -> $"Allele Frequency Input: {e}")

        open FsToolkit.ErrorHandling

        type Input =
            { GeneNameInput: GeneName.Input
              ResultGroup: ResultGroup.Input
              Interpretation: Interpretation.Input
              AlleleFrequency: AlleleFrequency.FrequencyInput option
              Source: Source.Input option }

        let validate input =
            validation {
                let! geneName = input.GeneNameInput |> GeneName.validate
                and! resultGroup = input.ResultGroup |> ResultGroup.validate
                and! interpretation = input.Interpretation |> Interpretation.validate
                and! source = input.Source |> Source.validate
                and! alleleFrequency = input.AlleleFrequency |> AlleleFrequency.Input.validate

                return { GeneName = geneName
                         ResultGroup = resultGroup
                         Interpretation = interpretation
                         AlleleFrequency = alleleFrequency
                         Source = source }
            }

    module GenomicAlterations =
        open Utilities

        let validate =
            Seq.map GenomicAlteration.validate
            >> Seq.toList
            >> Result.combine
            >> Result.mapError List.flatten

    type Report =
        { Test: Test
          Specimen: Specimen
          GenomicAlterations: GenomicAlteration seq
          Patient: Patient
          OrderingMd: OrderingMd
          Diagnosis: Diagnosis }

    module Report =
        open FSharp.Data
        open System.IO
        open System.Text.RegularExpressions
        open Utilities

        type Input =
            { TestInput: Test.Input
              PatientInput: Patient.Input
              OrderingMdInput: OrderingMd.Input
              DiagnosisInput: Diagnosis.Input
              GenomicAlterationInputs: GenomicAlteration.Input seq
              SpecimenInput: TumorSpecimen.Input }

        [<Literal>]
        let CarisReportXsdPath = __SOURCE_DIRECTORY__ + "/data/carisReport.xsd"

        type ReportProvider = XmlProvider<Schema=CarisReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.carisReport.xsd">

        /// A Caris Report XML file
        type Xml(filePath: string) =
            let filePath = filePath
            let text = File.ReadAllText(filePath)
            let report = ReportProvider.Parse(text)

            let testDetails = report.TestDetails
            let patientInfo = report.PatientInformation
            let tests = report.Tests
            let testResults = tests |> Seq.collect (fun test -> test.TestResults)


            (* Convenience methods for grabbing relevant parts of the xml document *)

            member _.TumorSpecimenInfo = report.SpecimenInformation.TumorSpecimenInformation
            member _.OrderedDate = testDetails.OrderedDate
            member _.ReceivedDate = testDetails.ReceivedDate
            member _.ReportId = testDetails.LabReportId
            member _.PhysicianInformation = report.PhysicianInformation

            member _.GenomicAlterations =
                testResults
                |> Seq.choose (fun testResult -> testResult.GenomicAlteration)
                |> Seq.map (fun ga ->
                    {| BiomarkerName = ga.BiomarkerNames |> Seq.head
                       GeneName = ga.Genes |> Seq.head
                       Result = ga.Results |> Seq.head
                       ResultGroup = ga.ResultGroups |> Seq.head
                       Source = ga.GenomicSources |> Seq.tryHead
                       Interpretation = ga.Interpretations |> Seq.head
                       AlleleFrequency = ga.AlleleFrequencyInformations |> Seq.tryHead |> Option.map (fun afi -> afi.AlleleFrequency) |})

            member _.ExpressionAlterations =
                testResults
                |> Seq.map (fun testResult -> testResult.ExpressionAlteration)
                |> Seq.choose id

            member _.CopyNumberAlterations =
                testResults
                |> Seq.choose (fun testResult -> testResult.CopyNumberAlteration)
                |> Seq.map (fun cna ->
                    {| BiomarkerName = cna.BiomarkerNames |> Seq.head
                       GeneName = cna.Genes |> Seq.head
                       ResultName = cna.Results |> Seq.head
                       ResultGroup = cna.ResultGroups |> Seq.head
                       CopyNumberType = cna.CopyNumberTypes |> Seq.tryHead |}
                )

            member _.FusionTranslocations =
                testResults
                |> Seq.choose (fun tr -> tr.Translocation)
                |> Seq.filter (fun tl -> tl.Results |> Seq.head |> String.matches(Regex("(Pathogenic Fusion|Fusion Detected)")))
                |> Seq.map (fun tl ->
                    {| Result = tl.Results |> Seq.head
                       ResultGroup = tl.ResultGroups |> Seq.head
                       Interpreation = tl.Interpretations |> Seq.head
                       FusionIsoForm = tl.FusionIsoForms |> Seq.head
                       Gene1 = tl.Gene1s |> Seq.head
                       Exon1 = tl.Exon1s |> Seq.head
                       Gene2 = tl.Gene2s |> Seq.head
                       Exon2 = tl.Exon2s |> Seq.head |}
                )

            (* Inputs to validate *)

            member _.TestInput : Test.Input =
                { LabNameInput = testDetails.LabName |> Test.LabName.Input
                  ReportIdInput = testDetails.LabReportId |> Test.ReportId.Input
                  OrderedDate = testDetails.OrderedDate |> Test.OrderedDate.Input
                  ReceivedDate = testDetails.ReceivedDate |> Test.ReceivedDate.Input }

            member _.PatientInput : Patient.Input =
                { LastName = LastName.Input patientInfo.LastName
                  FirstName = FirstName.Input patientInfo.FirstName
                  DateOfBirth = patientInfo.Dob
                  Sex = Patient.Sex.Input patientInfo.Gender
                  MrnInput = MRN.Input patientInfo.Mrn }

            member this.OrderingMdInput : OrderingMd.Input =
                let fullNameInput : FullName.Input =
                    { LastNameInput  = (LastName.Input this.PhysicianInformation.LastName)
                      FirstNameInput = (FirstName.Input this.PhysicianInformation.FirstName) }

                let npiInput = (NationalProviderId.Input this.PhysicianInformation.Npi)

                { NameInput = fullNameInput
                  NationalProviderIdInput = npiInput }

            member _.DiagnosisInput : Diagnosis.Input =
                { DiagnosisNameInput = Diagnosis.Name.Input patientInfo.Diagnosis
                  DiagnosisCodeInput = IcdCode.Input patientInfo.IcdCode
                  PathologicDiagnosisInput = Diagnosis.PathologicDiagnosis.Input patientInfo.PathologicDiagnosis
                  DiagnosisSiteInput = Diagnosis.Site.Input patientInfo.PrimarySite
                  LineageInput = Diagnosis.Lineage.Input patientInfo.Lineage
                  SubLineageInput = Diagnosis.SubLineage.Input patientInfo.SubLineage }

            member this.TumorSpecimenInput : TumorSpecimen.Input =
                { SpecimenIdInput = this.TumorSpecimenInfo.SpecimenId |> TumorSpecimen.SpecimenId.Input
                  AccessionIdInput = this.TumorSpecimenInfo.SpecimenAccessionId |> TumorSpecimen.AccessionId.Input
                  SpecimenType = this.TumorSpecimenInfo.SpecimenType |> TumorSpecimen.SpecimenType.Input
                  SpecimenSite = this.TumorSpecimenInfo.SpecimenSite |> TumorSpecimen.SpecimenSite.Input
                  CollectionDate = this.TumorSpecimenInfo.SpecimenCollectionDate |> CollectionDate
                  ReceivedDate = this.TumorSpecimenInfo.SpecimenReceivedDate |> ReceivedDate }

            member this.GenomicAlterationInputs : GenomicAlteration.Input seq =
                this.GenomicAlterations
                |> Seq.map (fun ga ->
                    { GeneNameInput = ga.GeneName |> GenomicAlteration.GeneName.Input
                      ResultGroup = { GroupInput = ga.ResultGroup; ResultInput = ga.Result }
                      Interpretation = ga.Interpretation |> GenomicAlteration.Interpretation.Input
                      AlleleFrequency = ga.AlleleFrequency |> Option.map GenomicAlteration.AlleleFrequency.FrequencyInput
                      Source = ga.Source |> Option.map GenomicAlteration.Source.Input }
                )

            /// The overall report input that encapsulates all the other inputs
            member this.ReportInput =
                { TestInput = this.TestInput
                  PatientInput = this.PatientInput
                  OrderingMdInput = this.OrderingMdInput
                  DiagnosisInput = this.DiagnosisInput
                  GenomicAlterationInputs = this.GenomicAlterationInputs
                  SpecimenInput = this.TumorSpecimenInput }

        open FsToolkit.ErrorHandling

        let validate input =
            validation {
                let! patient = Patient.validate input.PatientInput
                and! orderingMd = OrderingMd.validate input.OrderingMdInput
                and! genomicAlterations = GenomicAlterations.validate input.GenomicAlterationInputs
                and! specimen = TumorSpecimen.validate input.SpecimenInput
                and! test = Test.validate input.TestInput
                and! diagnosis = Diagnosis.validate input.DiagnosisInput

                return { Test = test
                         Patient = patient
                         OrderingMd = orderingMd
                         GenomicAlterations = genomicAlterations
                         Specimen = specimen
                         Diagnosis = diagnosis }
            }

    module Database =
        open Database

        /// Extract the patient from the report & prepare it as a database row.
        let toPatientRow (report: Report) =
            let patient = report.Patient
            let row = context.Public.Patients.Create()

            row.Mrn <- patient.MRN |> MRN.toInteger
            row.FirstName   <- patient.FirstName   |> FirstName.toString
            row.LastName    <- patient.LastName    |> LastName.toString
            row.DateOfBirth <- patient.DateOfBirth |> DateOfBirth.unwrap

            row

        let toVendorRow =
            let row = context.Public.Vendors.Create()

            // source: https://npiprofile.com/npi/1013973866
            row.Name <- "Caris Life Sciences"
            row.CliaNumber <- "03D1019490"
            row.StreetAddress <- "4610 SOUTH 44TH PLACE"
            row.City <- "Phoenix"
            row.State <- "Arizona"
            row.ZipCode <- "85040"

            row

        let toReportRow (report: Report) =
            let { Patient = patient; Test = test; OrderingMd = orderingMd } = report

            let row = context.Public.Reports.Create()
            let (ReportId reportId) = test.ReportId

            row.ReportId   <- reportId
            // row.IssuedDate <- test.ReceivedDate
            row.PatientMrn <- patient.MRN.Value

            row.OrderingPhysician       <- orderingMd.OrderingMdName |> FullName.toString |> Some
            row.OrderingPhysicianNumber <- orderingMd.NationalProviderId.Value |> Some

            row