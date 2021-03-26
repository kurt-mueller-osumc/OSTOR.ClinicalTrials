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
          DiagnosisCodes: IcdCode list
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

    type Pathologist =
        { Organization: PathologistOrganization option }
    and PathologistOrganization =
        internal
        | PathologistOrganization of string

        member this.Value =
            let (PathologistOrganization org) = this
            org

    /// The report's specimen/sample. Caris reports only list a tumor specimen.
    type Specimen =
        { SpecimenId: SpecimenId
          AccessionId: AccessionId
          SpecimenType: SpecimenType
          SpecimenSite: SpecimenSite
          CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate }
    and SpecimenId = internal SpecimenId of string
    and SpecimenSite = internal SpecimenSite of string
    and CollectionDate = internal CollectionDate of System.DateTime
    and ReceivedDate = internal ReceivedDate of System.DateTime
    and AccessionId = internal AccessionId of string
    and SpecimenType =
        internal
        | ``Tissue Biopsy Formalin Vial``
        | ``Tissue Biopsy Paraffin Blocks``
        | ``Tissue Biopsy Slide Unstained``

        member this.Value =
            match this with
            | ``Tissue Biopsy Formalin Vial`` -> "Tissue Biopsy Formalin Vial"
            | ``Tissue Biopsy Paraffin Blocks`` -> "Tissue Biopsy Paraffin Blocks"
            | ``Tissue Biopsy Slide Unstained`` -> "Tissue Biopsy Slide Unstained"

    type SpecimenId     with member this.Value = this |> fun (SpecimenId specimenId) -> specimenId
    type SpecimenSite   with member this.Value = this |> fun (SpecimenSite specimenSite) -> specimenSite
    type CollectionDate with member this.Value = this |> fun (CollectionDate collectionDate) -> collectionDate
    type ReceivedDate   with member this.Value = this |> fun (ReceivedDate receivedDate) -> receivedDate

    /// Test meta information
    type Test =
        { LabName: LabName
          OrderedDate: OrderedDate
          ReceivedDate: ReceivedDate
          ReportId: ReportId }
    and LabName     = internal LabName of string
    and OrderedDate = internal OrderedDate of System.DateTime
    and ReportId    = internal ReportId of string

    type LabName     with member this.Value = this |> fun (LabName labName)           -> labName
    type OrderedDate with member this.Value = this |> fun (OrderedDate orderedDate)   -> orderedDate
    type ReportId    with member this.Value = this |> fun (ReportId reportId)         -> reportId

    type GenomicAlteration =
        { GeneName: GeneName
          ResultGroup: GenomicAlterationResultGroup
          Source: GenomicAlterationSource option
          MolecularConsequence: MolecularConsequence option
          Interpretation: GeneInterpretation
          AlleleFrequency: AlleleFrequency option
          HGVS: HGVS
          TranscriptAlterationDetails: TranscriptAlterationDetails option }

        member this.IsSomatic =
            match this.Source with
            | Some Somatic -> true
            | _ -> false

        member this.HasMolecularConsequence =
            this.MolecularConsequence.IsSome

    and GeneInterpretation = internal GeneInterpretation of string
    and GenomicAlterationSource = internal | Somatic
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

    and MolecularConsequence =
        internal
        | Missense
        | Nonsense
        | Frameshift
        | Splicing
        | Noncoding
        | ``Codon Deletion``
        | ``Codon Insertion``
        | UTR
        | Promoter
        // special case where molecular consequence attribute is present with no value
        | BlankConsequence

    and TranscriptAlterationDetails =
        { ObservedNucleotide: Nucleotide
          ReferenceNucleotide: Nucleotide
          TranscriptId: TranscriptId
          TranscriptIdSource: TranscriptIdSource
          StartPosition: TranscriptStartPosition
          StopPosition: TranscriptStopPosition }
    and Nucleotide = internal Nucleotide of string
    and TranscriptId = internal TranscriptId of string
    and TranscriptIdSource = internal TranscriptIdSource of string
    and TranscriptStartPosition = internal TranscriptStartPosition of position: uint64
    and TranscriptStopPosition  = internal TranscriptStopPosition  of position: uint64

    and HGVS =
        internal
        | HGVS of {| CodingChange: HgvsCodingChange
                     ProteinChange: HgvsProteinChange |}
        | BlankHGVS
    and HgvsCodingChange  = internal | HgvsCodingChange of codingChange: string
    and HgvsProteinChange = internal | HgvsProteinChange of proteinChange: string


    (* Genomic Alteration Type Extensions *)

    type MolecularConsequence with
        /// The string representation of a molecular consequence
        member this.Value =
            match this with
            | Missense -> "missense"
            | Nonsense -> "nonsense"
            | Frameshift -> "frameshift"
            | Splicing -> "splicing"
            | Noncoding -> "noncoding"
            | ``Codon Deletion`` -> "codon deletion"
            | ``Codon Insertion`` -> "codon insertion"
            | UTR -> "utr"
            | Promoter -> "promoter"
            // special case where molecular consequence attribute is present with no value
            | BlankConsequence -> ""

    // Unwrap these single case unions to their string value
    type AlleleFrequency   with member this.Value = this |> fun (AlleleFrequency af) -> af
    type HgvsCodingChange  with member this.Value = this |> fun (HgvsCodingChange codingChange) -> codingChange
    type HgvsProteinChange with member this.Value = this |> fun (HgvsProteinChange proteinChange) -> proteinChange
    type TranscriptId      with member this.Value = this |> fun (TranscriptId transcriptId) -> transcriptId

    type HGVS with
        /// The string representation of an hgvs coding change
        member this.CodingChangeValue =
            match this with
            | HGVS hgvs -> hgvs.CodingChange.Value
            | BlankHGVS -> ""

        /// The string represenation of an hgvs protein change
        member this.ProteinChangeValue =
            match this with
            | HGVS hgvs -> hgvs.ProteinChange.Value
            | BlankHGVS -> ""

    type GenomicAlterationSource with
        member this.Value =
            match this with
            | Somatic -> "somatic"

    type GenomicAlteration with
        member this.MolecularConsequenceValue = this.MolecularConsequence |> Option.map (fun mc -> mc.Value) |> Option.defaultValue ""
        member this.SourceValue = this.Source |> Option.map (fun src -> src.Value) |> Option.defaultValue ""
        member this.TryTranscriptIdValue = this.TranscriptAlterationDetails |> Option.map (fun tad -> tad.TranscriptId.Value)

    /// Fusions are marked as "translocations" in a Caris report
    type Fusion =
        { Gene1Name: GeneName
          Gene2Name: GeneName
          Exon1: FusionExon
          Exon2: FusionExon
          Interpretation: FusionInterpretation
          FusionResult: FusionResult }
    and FusionExon = internal FusionExon of uint
    and FusionInterpretation = internal FusionInterpretation of string
    and FusionResult = internal ``Fusion Detected`` | ``Pathogenic Fusion``

    type FusionInterpretation with member this.Value = this |> fun (FusionInterpretation fusionInterpretation) -> fusionInterpretation

    type TumorMutationBurden =
        internal
        | IndeterminateTmb
        | LowTmb of int<mutation/megabase>
        | IntermediateTmb of int<mutation/megabase>
        | HighTmb of int<mutation/megabase>

        member this.TryValue =
            match this with
            | IndeterminateTmb -> None
            | LowTmb tmb -> Some (int tmb)
            | IntermediateTmb tmb -> Some (int tmb)
            | HighTmb tmb -> Some (int tmb)

    type MicrosatelliteInstability =
        internal
        | LowMSI
        | StableMSI
        | HighMSI
        | IndeterminateMSI

        member this.Value =
            match this with
            | LowMSI -> "low"
            | StableMSI -> "stable"
            | HighMSI -> "high"
            | IndeterminateMSI -> "indeterminate"

    module Patient =
        open FsToolkit.ErrorHandling

        module Sex =
            type Input = Input of string

            /// Validate that the input for a patient's sex is is either (M|m)ale or (F|f)emale
            let validate (Input input) =
                match input with
                | "Male" | "male" -> Ok Male
                | "Female" | "female" -> Ok Female
                | _ -> Error $"Sex is invalid: {input}"

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
        module Codes =
            open Utilities

            type Input = Input of string

            let validate (Input input) =
                input
                |> String.split ','
                |> List.map (IcdCode.Input >> IcdCode.validate)
                |> Result.combine

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
            { DiagnosisCodesInput: Codes.Input
              DiagnosisNameInput: Name.Input
              DiagnosisSiteInput: Site.Input
              PathologicDiagnosisInput: PathologicDiagnosis.Input
              LineageInput: Lineage.Input
              SubLineageInput: SubLineage.Input }

        open FsToolkit.ErrorHandling

        let validate (input: Input) =
            validation {
                let! icdCodes = input.DiagnosisCodesInput |> Codes.validate
                and! diagnosisName = input.DiagnosisNameInput |> Name.validate
                and! diagnosisSite = input.DiagnosisSiteInput |> Site.validate
                and! pathologicDiagnosis = input.PathologicDiagnosisInput |> PathologicDiagnosis.validate
                and! lineage = input.LineageInput |> Lineage.validate
                and! sublineage = input.SubLineageInput |> SubLineage.validate

                return { DiagnosisCodes = icdCodes
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

    module Pathologist =
        module Organization =
            type Input = Input of string

        type Input =
            { OrganizationInput: Organization.Input }

        /// Validate pathologist information. Caris Reports will only have the pathologist organization listed, if present.
        /// If the organization is not present, a blank string exists, which is still valid.
        let validate (input: Input) =
            let (Organization.Input orgInput) = input.OrganizationInput

            match orgInput with
            | "" -> Ok { Organization = None }
            | _ -> Ok <| { Organization = Some (PathologistOrganization orgInput) }

    /// Caris only contains a tumor specimen.
    module TumorSpecimen =
        module SpecimenId =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that the specimen id is not blank
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

    module Gene =
        module Interpretation =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map GeneInterpretation
                |> Result.mapError (fun _ -> $"Gene interpretation can't be blank")

    module Nucleotide =
        open System.Text.RegularExpressions

        type Input = Input of string

        // Validate that a nucleotide is either blank string, or is composed of only A, G, C, or T
        let validate (Input input) =
            if Regex("^(A|G|C|T)*$").Match(input).Success then
                Ok <| Nucleotide input
            else
                Error <| $"Invalid nucleotide(s): {input}"

    module HGVS =
        module CodingChange =
            type Input = Input of string

        module ProteinChange =
            type Input = Input of string

        /// Validate that HGVS coding and protein change are both empty or are both present.
        let validate (codingChangeInput: CodingChange.Input option) (proteinChangeInput: ProteinChange.Input option) =
            match codingChangeInput, proteinChangeInput with
            | None, None -> Ok BlankHGVS
            | Some (CodingChange.Input codingChange), Some (ProteinChange.Input proteinChange) -> Ok <| HGVS {| CodingChange = HgvsCodingChange codingChange; ProteinChange = HgvsProteinChange proteinChange |}
            | _ -> Error $"HGVS change error: {(codingChangeInput, proteinChangeInput)}"

    module GenomicAlteration =
        let isSomatic (genomicAlteration: GenomicAlteration) = genomicAlteration.IsSomatic
        let hasMolecularConsequence (genomicAlteration: GenomicAlteration) = genomicAlteration.HasMolecularConsequence

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

        module MolecularConsequence =
            type Input = Input of string

            /// Validate that the input for a molecular consequence is one of the following:
            /// 1. Blank
            /// 2. Missense
            /// 3. Nonsense
            /// 4. Frameshift
            /// 5. Noncoding
            /// 6. Codon deletion / insertion
            /// 7. Promoter
            let validate (Input input) =
                match input with
                | "" -> Ok BlankConsequence
                | "Missense" -> Ok Missense
                | "Nonsense" -> Ok Nonsense
                | "Frameshift" -> Ok Frameshift
                | "Noncoding" -> Ok Noncoding
                | "CODON_DELETION" -> Ok ``Codon Deletion``
                | "CODON_INSERTION" -> Ok ``Codon Insertion``
                | "Promoter" -> Ok Promoter
                | _ -> Error $"Invalid molecular consequence: {input}"

            /// Validate an optional input if it exists
            let validateOptional (input: Input option) =
                match input with
                | None -> Ok None
                | Some i ->
                    match validate i with
                    | Ok molecularConsequence -> Ok <| Some molecularConsequence
                    | Error e -> Error e

        module TranscriptAlterationDetails =
            module StartPosition =
                open Utilities

                type Input = Input of string

                /// Validate that a start position is a valid unsigned integer
                let validate (Input input) =
                    match UnsignedInteger64.tryParse input with
                    | Some position -> Ok <| TranscriptStartPosition position
                    | None -> Error $"Invalid start position: {input}"

            module StopPosition =
                open Utilities

                type Input = Input of string

                /// Validate that a stop position is a valid unsigned integer
                let validate (Input input) =
                    match UnsignedInteger64.tryParse input with
                    | Some position -> Ok <| TranscriptStopPosition position
                    | None -> Error $"Invalid start position: {input}"

            module TranscriptId =
                open System.Text.RegularExpressions

                type Input = Input of string

                /// Validate that a transcript id is either blank or starts with "NM_" followed by at least one digit.
                let validate (Input input) =
                    if Regex("^(NM_\d{1,})*$").Match(input).Success then
                        Ok <| TranscriptId input
                    else
                        Error $"Invalid transcript id: {input}"

            type Input =
                { StartPositionInput: StartPosition.Input
                  StopPositionInput: StopPosition.Input
                  TranscriptIdInput: TranscriptId.Input
                  ObservedNucleotideInput: Nucleotide.Input
                  ReferenceNucelotideInput: Nucleotide.Input  }

            open FsToolkit.ErrorHandling

            /// Validate input for transcript alteration detail
            let validate input =
                validation {
                    let! startPosition = input.StartPositionInput |> StartPosition.validate
                    and! stopPosition = input.StopPositionInput |> StopPosition.validate
                    and! transcriptId = input.TranscriptIdInput |> TranscriptId.validate
                    and! observedNucleotide = input.ObservedNucleotideInput |> Nucleotide.validate
                    and! referenceNucleotide = input.ReferenceNucelotideInput |> Nucleotide.validate

                    return { ObservedNucleotide = observedNucleotide
                             ReferenceNucleotide = referenceNucleotide
                             StartPosition = startPosition
                             StopPosition = stopPosition
                             TranscriptId = transcriptId
                             TranscriptIdSource = TranscriptIdSource "RefSeq" }
                }

            /// Validate optional input for transcript alteration detail
            let validateOptional (input: Input option) =
                match input with
                | None -> Ok None
                | Some input ->
                    match validate input with
                    | Ok tad -> Ok <| Some tad
                    | Error e -> Error e

        open FsToolkit.ErrorHandling

        type Input =
            { GeneNameInput: Gene.Name.Input
              Interpretation: Gene.Interpretation.Input
              ResultGroup: ResultGroup.Input
              MolecularConsequenceInput: MolecularConsequence.Input option
              TranscriptAlterationDetailsInput: TranscriptAlterationDetails.Input option
              HgvsCodingChangeInput: HGVS.CodingChange.Input option
              HgvsProteinChangeInput: HGVS.ProteinChange.Input option
              AlleleFrequency: AlleleFrequency.FrequencyInput option
              Source: Source.Input option }

        /// Validate that genomic alteration input is valid
        let validate input =
            validation {
                let! geneName = input.GeneNameInput |> Gene.Name.validate
                and! resultGroup = input.ResultGroup |> ResultGroup.validate
                and! interpretation = input.Interpretation |> Gene.Interpretation.validate
                and! source = input.Source |> Source.validate
                and! molecularConsequence = input.MolecularConsequenceInput |> MolecularConsequence.validateOptional
                and! alleleFrequency = input.AlleleFrequency |> AlleleFrequency.Input.validate
                and! transcriptAlterationDetails = input.TranscriptAlterationDetailsInput |> TranscriptAlterationDetails.validateOptional
                and! hgvsChange = (input.HgvsCodingChangeInput, input.HgvsProteinChangeInput) ||> HGVS.validate

                return { GeneName = geneName
                         ResultGroup = resultGroup
                         Interpretation = interpretation
                         AlleleFrequency = alleleFrequency
                         HGVS = hgvsChange
                         TranscriptAlterationDetails = transcriptAlterationDetails
                         MolecularConsequence = molecularConsequence
                         Source = source }
            }

    module GenomicAlterations =
        open Utilities

        /// Validate genomic alteration inputs
        let validate =
            Seq.map GenomicAlteration.validate
            >> Seq.toList
            >> Result.combine
            >> Result.mapError List.flatten

        /// Filter for only somatic genomic alterations
        let somatic (genomicAlterations: GenomicAlteration seq) =
            genomicAlterations |> Seq.filter GenomicAlteration.isSomatic

        let withMolecularConsequence (genomicAlterations: GenomicAlteration seq) =
            genomicAlterations |> Seq.filter GenomicAlteration.hasMolecularConsequence

    module Fusion =
        module Exon =
            open Utilities

            type Input = Input of string

            let validate (Input input) =
                match UnsignedInteger.tryParse input with
                | Some exon -> Ok <| FusionExon exon
                | _ -> Error $"Fusion exon not an unsigned integer: {input}"

        module Interpretation =
            open Utilities.StringValidations

            type Input = Input of string

            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map FusionInterpretation
                |> Result.mapError (fun _ -> "Fusion interpretation can't be blank")

        module Result =
            type Input = Input of string
            type GroupInput = GroupInput of string

            let validate (Input input) (GroupInput groupInput) =
                match input, groupInput with
                | "Pathogenic Fusion", "Mutated" ->  Ok ``Pathogenic Fusion``
                | "Fusion Detected", "Mutated" -> Ok ``Fusion Detected``
                | _ -> Error $"Invalid fusion result: {(input, groupInput)}"

        type Input =
            { Gene1NameInput: Gene.Name.Input
              Gene2NameInput: Gene.Name.Input
              Exon1Input: Exon.Input
              Exon2Input: Exon.Input
              InterpretationInput: Interpretation.Input
              ResultInput: Result.Input
              GroupInput: Result.GroupInput }

        open FsToolkit.ErrorHandling

        let validate input =
            validation {
                let! gene1Name = input.Gene1NameInput |> Gene.Name.validate
                and! gene2Name = input.Gene2NameInput |> Gene.Name.validate
                and! exon1 = input.Exon1Input |> Exon.validate
                and! exon2 = input.Exon2Input |> Exon.validate
                and! interpretation = input.InterpretationInput |> Interpretation.validate
                and! fusionResult = (input.ResultInput, input.GroupInput) ||> Result.validate

                return { Gene1Name = gene1Name
                         Gene2Name = gene2Name
                         Exon1 = exon1
                         Exon2 = exon2
                         Interpretation = interpretation
                         FusionResult = fusionResult }
            }

    module Fusions =
        open Utilities

        let validate (fusionInputs: Fusion.Input seq) =
            fusionInputs
            |> Seq.map Fusion.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten

    module TumorMutationBurden =
        module Score =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Attempt to convert a score input to a valid tmb score. If the regex is valid, convert it to an integer
            /// with the unit of measure: mutation/megabase.
            ///
            ///     (|ValidScore|_) (Input "3 per Mb") = Some (3 <mutation/megabase>)
            let (|ValidScore|_|) (Input input) =
                let m = Regex("^(?<score>\d{1,}) per Mb$").Match(input)

                if m.Success
                then Some (m.Groups.[1].Value |> int |> ((*) 1<mutation/megabase>))
                else None

        module Call =
            type Input = Input of string

        type Input =
            { ScoreInput: Score.Input
              CallInput: Call.Input }

        let validate input =
            match input.CallInput, input.ScoreInput with
            | (Call.Input "Indeterminate", Score.Input "") -> Ok IndeterminateTmb
            | (Call.Input "Low", Score.ValidScore tmbScore) -> Ok <| LowTmb tmbScore
            | (Call.Input "Intermediate", Score.ValidScore tmbScore) -> Ok <| IntermediateTmb tmbScore
            | (Call.Input "High", Score.ValidScore tmbScore) -> Ok <| HighTmb tmbScore
            | _ -> Error $"Tmb Score invalid: {input}"

        /// Validate an optional Tumor Mutation Burden input. If no input exists, it's valid.
        /// If an input exists, run validation checks on it.
        let validateOptional (input: Input option) =
            match input with
            | None -> Ok None
            | Some i ->
                match validate i with
                | Ok tmb -> Ok (Some tmb)
                | Error e -> Error e

    module MicrosatelliteInstability =
        module Call =
            type Input = Input of string

        module ResultGroup =
            type Input = Input of string

        type Input =
            { CallInput: Call.Input
              ResultGroupInput: ResultGroup.Input }

        let validate input =
            match input.CallInput, input.ResultGroupInput with
            | Call.Input "Stable", ResultGroup.Input "Normal" -> Ok StableMSI
            | Call.Input "High", ResultGroup.Input "High" -> Ok HighMSI
            | Call.Input "Indeterminate", ResultGroup.Input "No Result" -> Ok IndeterminateMSI
            | _ -> Error $"MSI input is invalid: {input}"

        let validateOptional (input: Input option) =
            match input with
            | None -> Ok None
            | Some i ->
                match validate i with
                | Ok msi -> Ok (Some msi)
                | Error e -> Error  e

    type Report =
        { Test: Test
          Specimen: Specimen
          GenomicAlterations: GenomicAlteration seq
          Fusions: Fusion seq
          Patient: Patient
          OrderingMd: OrderingMd
          Pathologist: Pathologist
          Diagnosis: Diagnosis
          TumorMutationBurden: TumorMutationBurden option
          MicrosatelliteInstability: MicrosatelliteInstability option }

        member this.SomaticGenomicAlterations =
            this.GenomicAlterations |> GenomicAlterations.somatic

        member this.GenomicAlterationsWithMolecularConsequence =
            this.GenomicAlterations |> GenomicAlterations.withMolecularConsequence

    module Report =
        open FSharp.Data
        open System.IO
        open System.Text.RegularExpressions
        open Utilities

        type Input =
            { TestInput: Test.Input
              PatientInput: Patient.Input
              OrderingMdInput: OrderingMd.Input
              PathologistInput: Pathologist.Input
              DiagnosisInput: Diagnosis.Input
              GenomicAlterationInputs: GenomicAlteration.Input seq
              FusionInputs: Fusion.Input seq
              SpecimenInput: TumorSpecimen.Input
              TumorMutationBurdenInput: TumorMutationBurden.Input option
              MicrosatelliteInstabilityInput: MicrosatelliteInstability.Input option }

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

            member _.PathologistInformation =
                let pi = report.PathologistInformation

                {| NPI = pi.Npi.XElement.Value
                   LastName = pi.LastName.XElement.Value
                   FirstName = pi.FirstName.XElement.Value
                   Organization = pi.Organization |}

            member _.TumorMutationBurden =
                testResults
                |> Seq.tryPick (fun testResult -> testResult.TumorMutationBurden)

            member _.MicrosatelliteInstability =
                testResults
                |> Seq.tryPick (fun testResult -> testResult.MicrosatelliteInstability)

            member _.GenomicAlterations =
                testResults
                |> Seq.choose (fun testResult -> testResult.GenomicAlteration)
                |> Seq.map (fun ga ->
                    {| BiomarkerName = ga.BiomarkerNames |> Seq.head
                       GeneName = ga.Genes |> Seq.head
                       Result = ga.Results |> Seq.head
                       ResultGroup = ga.ResultGroups |> Seq.head
                       HgvsCodingChange = ga.HgvsCodingChanges |> Seq.tryHead
                       HgvsProteinChange = ga.HgvsProteinChanges |> Seq.tryHead
                       Chromosome = ga.Chromosomes |> Seq.tryHead
                       Exon = ga.Exons |> Seq.tryHead
                       Source = ga.GenomicSources |> Seq.tryHead
                       Interpretation = ga.Interpretations |> Seq.head
                       AlleleFrequency = ga.AlleleFrequencyInformations |> Seq.tryHead |> Option.map (fun afi -> afi.AlleleFrequency)
                       MolecularConsequence = ga.MolecularConsequences |> Seq.tryHead
                       TranscriptAlterationDetail = ga.AlterationDetails
                                                    |> Seq.tryHead
                                                    |> Option.map (fun ad ->
                                                        let tad = ad.TranscriptAlterationDetails
                                                        {| ReferenceNucleotide = tad.ReferenceNucleotide
                                                           ObservedNucleotide = tad.ObservedNucleotide
                                                           TranscriptStartPosition = tad.TranscriptStartPosition
                                                           TranscriptStopPosition = tad.TranscriptStopPosition
                                                           TranscriptId = tad.TranscriptId
                                                           TranscriptIdSource = tad.TranscriptIdSource
                                                        |})
                    |})

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
                       Interpretation = tl.Interpretations |> Seq.head
                       FusionIsoForm = tl.FusionIsoForms |> Seq.head
                       Gene1 = tl.Gene1s |> Seq.head
                       Exon1 = tl.Exon1s |> Seq.head
                       Gene2 = tl.Gene2s |> Seq.head
                       Exon2 = tl.Exon2s |> Seq.head |})


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

            member this.PathologistInput : Pathologist.Input =
                { OrganizationInput = (Pathologist.Organization.Input this.PathologistInformation.Organization) }

            member _.DiagnosisInput : Diagnosis.Input =
                { DiagnosisNameInput = Diagnosis.Name.Input patientInfo.Diagnosis
                  DiagnosisCodesInput = Diagnosis.Codes.Input patientInfo.IcdCode
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

            member this.TmbInput : TumorMutationBurden.Input option =
                this.TumorMutationBurden
                |> Option.map (fun tmb ->
                    { CallInput = tmb.MutationBurdenCall |> TumorMutationBurden.Call.Input
                      ScoreInput = tmb.MutationBurdenScore |> TumorMutationBurden.Score.Input
                    }
                )

            member this.MsiInput : MicrosatelliteInstability.Input option =
                this.MicrosatelliteInstability
                |> Option.map (fun msi ->
                    { CallInput = msi.MsiCall |> MicrosatelliteInstability.Call.Input
                      ResultGroupInput = msi.ResultGroup |> MicrosatelliteInstability.ResultGroup.Input }
                )

            member this.GenomicAlterationInputs : GenomicAlteration.Input seq =
                this.GenomicAlterations
                |> Seq.filter (fun ga -> ga.Result <> "variantnotdetected")
                |> Seq.map (fun ga ->
                    { GeneNameInput = ga.GeneName |> Gene.Name.Input
                      ResultGroup = { GroupInput = ga.ResultGroup; ResultInput = ga.Result }
                      Interpretation = ga.Interpretation |> Gene.Interpretation.Input
                      AlleleFrequency = ga.AlleleFrequency |> Option.map GenomicAlteration.AlleleFrequency.FrequencyInput
                      Source = ga.Source |> Option.map GenomicAlteration.Source.Input
                      MolecularConsequenceInput = ga.MolecularConsequence |> Option.map GenomicAlteration.MolecularConsequence.Input
                      HgvsCodingChangeInput = ga.HgvsCodingChange |> Option.map HGVS.CodingChange.Input
                      HgvsProteinChangeInput = ga.HgvsProteinChange |> Option.map HGVS.ProteinChange.Input
                      TranscriptAlterationDetailsInput = ga.TranscriptAlterationDetail |> Option.map (fun tad ->
                           { StartPositionInput = tad.TranscriptStartPosition |> GenomicAlteration.TranscriptAlterationDetails.StartPosition.Input
                             StopPositionInput  = tad.TranscriptStopPosition |> GenomicAlteration.TranscriptAlterationDetails.StopPosition.Input
                             TranscriptIdInput  = tad.TranscriptId |> GenomicAlteration.TranscriptAlterationDetails.TranscriptId.Input
                             ObservedNucleotideInput  = tad.ObservedNucleotide |> Nucleotide.Input
                             ReferenceNucelotideInput = tad.ReferenceNucleotide |> Nucleotide.Input
                           })
                    })

            member this.FusionInputs : Fusion.Input seq =
                this.FusionTranslocations
                |> Seq.map (fun ft ->
                    { Gene1NameInput = ft.Gene1 |> Gene.Name.Input
                      Gene2NameInput = ft.Gene2 |> Gene.Name.Input
                      Exon1Input = ft.Exon1 |> Fusion.Exon.Input
                      Exon2Input = ft.Exon2 |> Fusion.Exon.Input
                      InterpretationInput = ft.Interpretation |> Fusion.Interpretation.Input
                      ResultInput = ft.Result |> Fusion.Result.Input
                      GroupInput = ft.ResultGroup |> Fusion.Result.GroupInput }
                )

            /// The overall report input that encapsulates all the other inputs
            member this.ReportInput =
                { TestInput = this.TestInput
                  PatientInput = this.PatientInput
                  OrderingMdInput = this.OrderingMdInput
                  PathologistInput = this.PathologistInput
                  DiagnosisInput = this.DiagnosisInput
                  GenomicAlterationInputs = this.GenomicAlterationInputs
                  FusionInputs = this.FusionInputs
                  SpecimenInput = this.TumorSpecimenInput
                  TumorMutationBurdenInput = this.TmbInput
                  MicrosatelliteInstabilityInput = this.MsiInput }

        open FsToolkit.ErrorHandling

        /// Validate the report and its inputs. This either returns a report or a list of errors.
        let validate input =
            validation {
                let! patient = Patient.validate input.PatientInput
                and! orderingMd = OrderingMd.validate input.OrderingMdInput
                and! pathologistInfo = Pathologist.validate input.PathologistInput
                and! genomicAlterations = GenomicAlterations.validate input.GenomicAlterationInputs
                and! fusions = Fusions.validate input.FusionInputs
                and! specimen = TumorSpecimen.validate input.SpecimenInput
                and! test = Test.validate input.TestInput
                and! diagnosis = Diagnosis.validate input.DiagnosisInput
                and! tmb = TumorMutationBurden.validateOptional input.TumorMutationBurdenInput
                and! msi = MicrosatelliteInstability.validateOptional input.MicrosatelliteInstabilityInput

                return { Test = test
                         Patient = patient
                         OrderingMd = orderingMd
                         Pathologist = pathologistInfo
                         GenomicAlterations = genomicAlterations
                         Fusions = fusions
                         Specimen = specimen
                         Diagnosis = diagnosis
                         TumorMutationBurden = tmb
                         MicrosatelliteInstability = msi }
            }

    module Database =
        open Database

        /// Convert report's patient information to a patient database row.
        let toPatientRow (report: Report) =
            let patient = report.Patient
            let row = context.Public.Patients.Create()

            row.Mrn <- patient.MRN |> MRN.toInteger
            row.FirstName   <- patient.FirstName   |> FirstName.toString
            row.LastName    <- patient.LastName    |> LastName.toString
            row.DateOfBirth <- patient.DateOfBirth |> DateOfBirth.unwrap

            row

        /// Prepare a row to be created in the "vendors" table for Caris Life Sciences
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

        /// Prepare a database row in the "reports" table
        let toReportRow (report: Report) =
            let { Patient = patient
                  Diagnosis = diagnosis
                  Test = test
                  OrderingMd = orderingMd
                  Pathologist = pathologist } = report

            let row = context.Public.Reports.Create()
            let (ReportId reportId) = test.ReportId

            // overall report info
            row.VendorCliaNumber <- "03D1019490"
            row.ReportId   <- reportId
            row.PatientMrn <- patient.MRN.Value
            row.IssuedDate <- test.ReceivedDate.Value

            // ordering physician
            row.OrderingPhysician       <- orderingMd.OrderingMdName |> FullName.toString |> Some
            row.OrderingPhysicianNumber <- orderingMd.NationalProviderId.Value |> Some

            // pathologist - only organizations get listed in caris reports
            row.Pathologist <- pathologist.Organization |> Option.map (fun org -> org.Value)

            // diagnosis - caris doesn't report diagnosis dates
            row.DiagnosisName       <- diagnosis.DiagnosisName.Value
            row.DiagnosisIcd10Codes <- diagnosis.DiagnosisCodes |> List.map IcdCode.toString |> List.toArray |> Some

            // biomarkers
            row.TumorMutationalBurden <- report.TumorMutationBurden |> Option.bind (fun tmb -> tmb.TryValue)
            row.MsiStatus <- report.MicrosatelliteInstability |> Option.map (fun msi -> msi.Value)

            row

        /// Each report lists a sample that may be referred to across reports. Therefore, samples are given their own table and listings of a sample in a report are givne their own table.
        ///
        /// Only tumor samplesa are listed in Caris reports.
        let toSampleRow (report: Report) =
            let specimen = report.Specimen
            let row = context.Public.Samples.Create()

            row.SampleId       <- specimen.SpecimenId.Value
            row.SampleType     <- specimen.SpecimenType.Value
            row.Category       <- "tumor"
            row.BiopsySite     <- specimen.SpecimenSite.Value

            row

        /// The parent sample and report must exist in the datbase.
        let toSampleReportRow (report: Report) =
            let specimen = report.Specimen
            let test = report.Test
            let row = context.Public.SampleReports.Create()

            row.CollectionDate <- specimen.CollectionDate.Value
            row.ReceiptDate    <- specimen.ReceivedDate.Value
            row.ReportId       <- test.ReportId.Value

            row

        /// Convert all genomic alterations with molecular consequences to 'gene' database rows
        let toGeneRows (report: Report) =
            report.GenomicAlterationsWithMolecularConsequence
            |> Seq.map (fun ga ->
                let row = context.Public.Genes.Create()

                row.Name <- ga.GeneName.Value

                row
            )

        /// Convert a report's genomic alterations that have a 'molecular consequence' to 'Variant' database rows.
        /// Each genomic alteration with a "molecular consequence" will also have an hgvs coding change and protein change.
        ///
        /// Each variant row has a parent sample report and a parent gene. The parent sample report and gene must already exist in the database.
        let toVariantRows (report: Report) =
            // Find the existing sample report based on this report's report id and sample id
            let sampleReportId =
                query {
                    for sampleReport in context.Public.SampleReports do
                    where (sampleReport.ReportId = report.Test.ReportId.Value && sampleReport.SampleId = report.Specimen.SpecimenId.Value)
                    select sampleReport.Id
                } |> Seq.head

            report.GenomicAlterationsWithMolecularConsequence
            |> Seq.map (fun ga ->
                let row = context.Public.Variants.Create()

                // Associate the variant to the gene by its name
                row.GeneName       <- ga.GeneName.Value
                row.SampleReportId <- sampleReportId
                row.Name           <- ga.HGVS.ProteinChangeValue

                row.AllelicFraction <- ga.AlleleFrequency |> Option.map (fun af -> float af.Value)
                row.Category <- ga.SourceValue // is 'somatic'
                row.Type <- ga.MolecularConsequenceValue |> Some

                row.HgvsC <- ga.HGVS.CodingChangeValue |> Some
                row.HgvsProtein <- ga.HGVS.ProteinChangeValue |> Some
                row.Transcript <- ga.TryTranscriptIdValue

                row
            )

        let toFusionGeneRows (report: Report) =
            report.Fusions
            |> Seq.collect (fun fusion ->
                let gene1Row = context.Public.Genes.Create()
                let gene2Row = context.Public.Genes.Create()

                gene1Row.Name <- fusion.Gene1Name.Value
                gene2Row.Name <- fusion.Gene2Name.Value

                [gene1Row; gene2Row]
            )

        let toFusionRows (report: Report) =
            let reportId = report.Test.ReportId.Value

            report.Fusions
            |> Seq.map (fun fusion ->
                let row = context.Public.Fusions.Create()

                row.ReportId <- reportId
                row.FirstGeneName  <- fusion.Gene1Name.Value
                row.SecondGeneName <- fusion.Gene2Name.Value
                row.Description    <- fusion.Interpretation.Value
                row.FusionType <- "mutation"

                row
            )