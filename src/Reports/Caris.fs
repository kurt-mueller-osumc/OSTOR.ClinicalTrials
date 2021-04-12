namespace OSTOR.ClinicalTrials.Reports

module Caris =
    [<AutoOpen>]
    module Domain =
        open Core

        type Patient =
            { MRN: Patient.MRN option
              LastName: Person.LastName
              FirstName: Person.FirstName
              DateOfBirth: Person.DateOfBirth
              Sex: Sex }

            member this.TryMrnValue = this.MRN |> Option.map (fun mrn -> mrn.Value)
            member this.HasMRN = this.MRN.IsSome

        and Sex = internal Male | Female

        type Diagnosis =
            { DiagnosisName: Diagnosis.Name
              DiagnosisCodes: DiagnosisCodes
              PathologicDiagnosis: PathologicDiagnosis
              DiagnosisSite: DiagnosisSite
              Lineage: Lineage
              SubLineage: SubLineage }

        and DiagnosisCodes =
            internal | DiagnosisCodes of IcdCode list

            member this.Values = this |> fun (DiagnosisCodes codes) -> codes |> List.map (fun code -> code.Value)

        and PathologicDiagnosis = internal PathologicDiagnosis of string
        and DiagnosisSite = internal DiagnosisSite of string
        and Lineage = internal Lineage of string
        and SubLineage = internal SubLineage of string

        type OrderingMd =
            { Name: Person.FullName
              NationalProviderId: NationalProviderId }

        module Pathologist =
            type Organization =
                internal | Organization of string
                member this.Value = this |> fun (Organization org) -> org

        type Pathologist =
            { Organization: Pathologist.Organization option }
            member this.TryOrganizationValue =
                this.Organization |> Option.map (fun org -> org.Value)

        type CollectionDate =
            internal | CollectionDate of System.DateTime
            member this.Value = this |> fun (CollectionDate collectionDate) -> collectionDate
        type ReceivedDate =
            internal | ReceivedDate of System.DateTime
            member this.Value = this |> fun (ReceivedDate receivedDate) -> receivedDate

        module Specimen =
            type Identifier =
                internal | Identifier of string
                member this.Value = this |> fun (Identifier identifier) -> identifier
            type Site =
                internal | Site of string
                member this.Value = this |> fun (Site site) -> site

            type Type =
                internal
                | ``Peripheral Blood Plasma``
                | ``Tissue Biopsy Formalin Vial``
                | ``Tissue Biopsy Paraffin Blocks``
                | ``Tissue Biopsy Slide Unstained``

                member this.Value =
                    match this with
                    | ``Peripheral Blood Plasma`` -> "Peripheral Blood Plasma"
                    | ``Tissue Biopsy Formalin Vial`` -> "Tissue Biopsy Formalin Vial"
                    | ``Tissue Biopsy Paraffin Blocks`` -> "Tissue Biopsy Paraffin Blocks"
                    | ``Tissue Biopsy Slide Unstained`` -> "Tissue Biopsy Slide Unstained"

        /// The report's specimen/sample. Caris reports only list a tumor specimen.
        type Specimen =
            { SpecimenId: Specimen.Identifier
              AccessionId: AccessionId
              Type: Specimen.Type
              Site: Specimen.Site
              CollectionDate: CollectionDate
              ReceivedDate: ReceivedDate }
        and AccessionId = internal AccessionId of string

        /// Test meta information
        type Test =
            { LabName: LabName
              OrderedDate: OrderedDate
              ReceivedDate: ReceivedDate
              ReportId: ReportId }
        and ReportId =
            internal | ReportId of string
            member this.Value = this |> fun (ReportId reportId) -> reportId
        and OrderedDate =
            internal | OrderedDate of System.DateTime
            member this.Value = this |> fun (OrderedDate orderedDate) -> orderedDate

        module HGVS =
            type CodingChange =
                internal | CodingChange of codingChange: string
                member this.Value = this |> fun (CodingChange codingChange) -> codingChange

            type ProteinChange =
                internal | ProteinChange of proteinChange: string
                member this.Value = this |> fun (ProteinChange proteinChange) -> proteinChange

        type HGVS =
            { CodingChange: HGVS.CodingChange
              ProteinChange: HGVS.ProteinChange }

        module GenomicAlteration =
            /// Genomic alterations will be grouped as either:
            /// - mutated
            /// - no result
            /// - normal
            type ResultGroup =
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

            type Source =
                internal | Somatic

                member this.IsSomaticSource =
                    match this with
                    | Somatic -> true

                member this.Value =
                    match this with
                    | Somatic -> "somatic"

            type MolecularConsequence =
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

            type GeneInterpretation = internal GeneInterpretation of string
            type AlleleFrequency =
                internal | AlleleFrequency of uint
                member this.Value = this |> fun (AlleleFrequency alleleFrequency) -> alleleFrequency

            module Transcript =
                type Identifier =
                    internal | Identifier of string
                    member this.Value = this |> fun (Identifier identifier) -> identifier

                type IdSource = internal IdSource of string
                type StartPosition = internal StartPosition of position: uint64
                type StopPosition  = internal StopPosition  of position: uint64

            type TranscriptAlterationDetails =
                { ObservedNucleotide: Nucleotide
                  ReferenceNucleotide: Nucleotide
                  Identifier: Transcript.Identifier
                  IdSource: Transcript.IdSource
                  StartPosition: Transcript.StartPosition
                  StopPosition: Transcript.StopPosition }
            and Nucleotide = internal Nucleotide of string

        type GenomicAlteration =
            { GeneName: Gene.Name
              ResultGroup: GenomicAlteration.ResultGroup
              GenomicSource: GenomicAlteration.Source option
              MolecularConsequence: GenomicAlteration.MolecularConsequence option
              Interpretation: GenomicAlteration.GeneInterpretation option
              AlleleFrequency: GenomicAlteration.AlleleFrequency option
              HGVS: HGVS option
              TranscriptAlterationDetails: GenomicAlteration.TranscriptAlterationDetails option }

            member this.IsSomatic =
                this.GenomicSource
                |> Option.map (fun source -> source.IsSomaticSource)
                |> Option.defaultValue false

            member this.HasMolecularConsequence =
                this.MolecularConsequence.IsSome

            member this.MolecularConsequenceValue = this.MolecularConsequence |> Option.map (fun mc -> mc.Value) |> Option.defaultValue ""
            member this.SourceValue = this.GenomicSource |> Option.map (fun src -> src.Value) |> Option.defaultValue ""
            member this.TryTranscriptIdValue = this.TranscriptAlterationDetails |> Option.map (fun tad -> tad.Identifier.Value)
            member this.TryHgvsProteinChangeValue = this.HGVS |> Option.map (fun hgvs -> hgvs.ProteinChange.Value)
            member this.TryHgvsCodingChangeValue  = this.HGVS |> Option.map (fun hgvs -> hgvs.CodingChange.Value)
            member this.TryAlleleFrequencyValue = this.AlleleFrequency |> Option.map (fun alleleFrequency -> alleleFrequency.Value)

        module Fusion =
            type Exon = internal Exon of uint
            type Interpretation =
                internal | Interpretation of string
                member this.Value = this |> fun (Interpretation fusionInterpretation) -> fusionInterpretation
            type Result = internal ``Fusion Detected`` | ``Pathogenic Fusion``

        /// Fusions are marked as "translocations" in a Caris report
        type Fusion =
            { Gene1Name: Gene.Name
              Gene2Name: Gene.Name
              Exon1: Fusion.Exon
              Exon2: Fusion.Exon
              Interpretation: Fusion.Interpretation
              Result: Fusion.Result }

            member this.GeneNames = [this.Gene1Name; this.Gene2Name]

        module TumorMutationBurden =
            type Score =
                internal
                | Indeterminate
                | Low of int<mutation/megabase>
                | Intermediate of int<mutation/megabase>
                | High of int<mutation/megabase>

                member this.TryValue =
                    match this with
                    | Indeterminate -> None
                    | Low tmb -> Some (int tmb)
                    | Intermediate tmb -> Some (int tmb)
                    | High tmb -> Some (int tmb)

        module MicrosatelliteInstability =
            type Status =
                internal
                | Low
                | Stable
                | High
                | Indeterminate

                member this.Value =
                    match this with
                    | Low -> "low"
                    | Stable -> "stable"
                    | High -> "high"
                    | Indeterminate -> "indeterminate"

        type Report =
            { Test: Test
              Specimen: Specimen
              GenomicAlterations: GenomicAlteration seq
              Fusions: Fusion seq
              Patient: Patient
              OrderingMd: OrderingMd
              Pathologist: Pathologist
              Diagnosis: Diagnosis
              TumorMutationBurden: TumorMutationBurden.Score option
              MicrosatelliteInstability: MicrosatelliteInstability.Status option }

            member this.SomaticGenomicAlterations =
                this.GenomicAlterations |> Seq.filter (fun ga -> ga.IsSomatic)

            member this.GenomicAlterationsWithMolecularConsequence =
                this.GenomicAlterations |> Seq.filter (fun ga -> ga.HasMolecularConsequence)

    module Input =
        open Core

        type Patient =
            { MRN: string
              LastName: string
              FirstName: string
              DateOfBirth: System.DateTime
              Sex: string }

        module Patient =
            open FsToolkit.ErrorHandling

            module Sex =
                open type Sex

                /// Validate that the input for a patient's sex is is either (M|m)ale or (F|f)emale
                let validate input =
                    match input with
                    | "Male" | "male" -> Ok Male
                    | "Female" | "female" -> Ok Female
                    | _ -> Error $"Sex is invalid: {input}"

            module MRN =
                open Utilities.StringValidations

                let validate str =
                    match str with
                    | "" -> Ok None
                    | _ -> str |> Input.Patient.MRN.validate |> Result.map Some

            open Core.Input

            /// Validate that a report's patient has a valid mrn, name, and sex
            let validate (patient: Patient) =
                validation {
                    let! mrn = patient.MRN |> MRN.validate
                    and! lastName =  patient.LastName |> Person.LastName.validate
                    and! firstName =  patient.FirstName |> Person.FirstName.validate
                    and! sex = patient.Sex |> Sex.validate

                    let dob = Person.DateOfBirth patient.DateOfBirth

                    return ({
                        MRN = mrn
                        LastName = lastName
                        FirstName = firstName
                        Sex = sex
                        DateOfBirth = dob
                    } : Domain.Patient)
                }

        type Diagnosis =
            { Codes: string // string that contains comma-separated icd codes
              Name: string
              Site: string
              PathologicDiagnosis: string
              Lineage: string
              SubLineage: string }

        module Diagnosis =
            module Codes =
                open Utilities
                open Core.Input

                /// Validate a string that contains comma-separated icd codes
                let validate =
                    String.split ','
                    >> List.map IcdCode.validate
                    >> Result.combine
                    >> Result.map DiagnosisCodes

            open Utilities.StringValidations.Typed

            module Name =
                /// Validate that diagnosis name is not blank
                let validate = validateNotBlank Diagnosis.Name "Diagnosis name can't be blank"

            module Site =
                /// Validate that diagnosis site is not blank
                let validate = validateNotBlank DiagnosisSite "Diagnosis site can't be blank"

            module PathologicDiagnosis =
                /// Validate that diagnosis site is not blank
                let validate = validateNotBlank PathologicDiagnosis "Pathologic diagnosis site can't be blank"

            module Lineage =
                /// Validate that lineage is not blank
                let validate = validateNotBlank Lineage "Lineage can't be blank"

            module SubLineage =
                /// Validate that sublineage is not blank
                let validate = validateNotBlank SubLineage "SubLineage can't be blank"

            open FsToolkit.ErrorHandling

            /// Validate that a diagnosis has valid icd codes, name, site, pathologic diagnosis, lineage, and sublineage
            let validate (diagnosis: Diagnosis) =
                validation {
                    let! icdCodes = diagnosis.Codes |> Codes.validate
                    and! diagnosisName = diagnosis.Name |> Name.validate
                    and! diagnosisSite = diagnosis.Site |> Site.validate
                    and! pathologicDiagnosis = diagnosis.PathologicDiagnosis |> PathologicDiagnosis.validate
                    and! lineage = diagnosis.Lineage |> Lineage.validate
                    and! sublineage = diagnosis.SubLineage |> SubLineage.validate

                    return ({
                        DiagnosisCodes = icdCodes
                        DiagnosisName = diagnosisName
                        DiagnosisSite = diagnosisSite
                        PathologicDiagnosis = pathologicDiagnosis
                        Lineage = lineage
                        SubLineage = sublineage } : Domain.Diagnosis)
                }

        open Core.Input

        type OrderingMd =
            { FullName: Person.FullName
              NationalProviderId: string }

        module OrderingMd =
            open FsToolkit.ErrorHandling

            /// Validate the presence an ordering md's name and the format of their national provider id
            let validate orderingMd =
                validation {
                    let! name = orderingMd.FullName |> Person.FullName.validate
                    and! npi = orderingMd.NationalProviderId |> NationalProviderId.validate

                    return ({
                        Name = name
                        NationalProviderId = npi
                    } : Domain.OrderingMd)
                }

        type Pathologist =
            { Organization: string }

        module Pathologist =
            open type Pathologist.Organization

            /// Validate pathologist information. Caris Reports will only have the pathologist organization listed, if present.
            /// If the organization is not present, a blank string exists, which is still valid.
            let validate (pathologist: Pathologist) : Result<Domain.Pathologist,string> =
                match pathologist.Organization with
                | "" -> Ok { Organization = None }
                | _ -> Ok <| { Organization = Some (Organization pathologist.Organization) }

        type TumorSpecimen =
            { SpecimenId: string
              AccessionId: string
              Type: string
              Site: string
              CollectionDate: System.DateTime
              ReceivedDate: System.DateTime }

        /// Caris only contains a tumor specimen.
        module TumorSpecimen =
            module SpecimenId =
                open Utilities.StringValidations.Typed

                /// Validate that the specimen id is not blank
                let validate = validateNotBlank Specimen.Identifier "Sample id cannot be blank"


            module AccessionId =
                open System.Text.RegularExpressions

                /// Validate that a sample's accession id is in the following format where `d` is a digit and `A` is any letter A-Z: `TNdd-dddddd-A`
                ///
                ///    validate "TN21-123456-A" = Ok (AccessionId "TN21-123456-A")
                ///    validate "invalidId" = Error "Invalid accession id: invalidId"
                let validate input =
                    if Regex("^TN\d{2}-\d{6}-[A-Z]{1}\d*$").Match(input).Success then
                        Ok <| AccessionId input
                    else
                        Error $"Invalid accession id: {input}"

            module Type =
                open type Specimen.Type

                /// Validate that a sample type's tissue biopsy is from a vial, blocks, or a slide.
                let validate input =
                    match input with
                    | "Peripheral Blood Plasma" -> Ok ``Peripheral Blood Plasma``
                    | "Tissue Biopsy Formalin Vial" -> Ok ``Tissue Biopsy Formalin Vial``
                    | "Tissue Biopsy Paraffin Blocks" -> Ok ``Tissue Biopsy Paraffin Blocks``
                    | "Tissue Biopsy Slide Unstained" -> Ok ``Tissue Biopsy Slide Unstained``
                    | _ -> Error $"Unknown specimen type: {input}"

            module Site =
                open Utilities.StringValidations.Typed
                open type Specimen.Site

                /// Validate that a specimen site is not blank
                let validate = validateNotBlank Site "Specimen site cannot be blank"

            open FsToolkit.ErrorHandling
            open type CollectionDate
            open type ReceivedDate


            let validate sample =
                validation {
                    let! specimenId = SpecimenId.validate sample.SpecimenId
                    and! accessionId = AccessionId.validate sample.AccessionId
                    and! specimenType = Type.validate sample.Type
                    and! specimenSite = Site.validate sample.Site

                    let collectionDate = CollectionDate sample.CollectionDate
                    let receivedDate = ReceivedDate sample.ReceivedDate

                    return ({
                        SpecimenId = specimenId
                        AccessionId = accessionId
                        Type = specimenType
                        Site = specimenSite
                        CollectionDate = collectionDate
                        ReceivedDate = receivedDate
                    } : Domain.Specimen)
                }

        type Test =
            { LabName: string
              ReportId: string
              OrderedDate: string
              ReceivedDate: string }

        module Test =
            open FsToolkit.ErrorHandling
            open Utilities.StringValidations.Typed

            module OrderedDate =
                open type OrderedDate
                /// Validate the ordered date for a test
                let validate input =
                    input |> validateDateTime OrderedDate $"Invalid ordered date: {input}"

            module ReceivedDate =
                open type ReceivedDate

                /// Validate the received date for a test
                let validate input =
                    input |> validateDateTime ReceivedDate $"Invalid received date: {input}"

            module ReportId =
                open System.Text.RegularExpressions

                /// Validate that a report id is in the following format where `d` is a digit: `TNdd-dddddd`
                ///
                ///    validate "TN12-123456" = Ok (Domain.ReportId "TN12-123456")
                let validate input =
                    if Regex("^TN\d{2}-\d{6}$").Match(input).Success then
                        Ok <| Domain.ReportId input
                    else Error $"ReportId - Invalid report id: {input}"

            /// Validate that a test has a report id, ordered date, received date, and lab name
            let validate test =
                validation {
                    let! reportId = ReportId.validate test.ReportId
                    and! orderedDate = OrderedDate.validate test.OrderedDate
                    and! receivedDate = ReceivedDate.validate test.ReceivedDate
                    and! labName = Lab.Name.validate test.LabName

                    return ({
                        ReportId = reportId
                        LabName = labName
                        OrderedDate = orderedDate
                        ReceivedDate = receivedDate
                    } : Domain.Test)
                }

        module Gene =
            module Interpretation =
                open Utilities
                open Utilities.StringValidations.Typed

                /// Validate that a gene interpretation is not blank
                let validate = validateNotBlank GenomicAlteration.GeneInterpretation "Gene interpretation can't be blank"

                /// Validate a genomic alteration's interpretation, if it exists
                let validateOptional = Optional.validateWith validate

        module Nucleotide =
            open System.Text.RegularExpressions
            // Validate that a nucleotide is either blank string, or is composed of only A, G, C, or T
            let validate input =
                if Regex("^(A|G|C|T)*$").Match(input).Success then
                    Ok <| GenomicAlteration.Nucleotide input
                else
                    Error <| $"Invalid nucleotide(s): {input}"

        type HGVS =
            { CodingChange: string option
              ProteinChange: string option }

        module HGVS =
            open type HGVS.CodingChange
            open type HGVS.ProteinChange

            /// Validate that HGVS coding and protein change are both empty or are both present.
            let validate (hgvs: HGVS) : Result<Domain.HGVS option, string> =
                match hgvs.CodingChange, hgvs.ProteinChange with
                | None, None -> Ok None
                | Some codingChange, Some proteinChange ->
                    Ok <| Some { CodingChange  = CodingChange codingChange
                                 ProteinChange = ProteinChange proteinChange }
                | _ -> Error $"Invalid HGVS: {hgvs}"

        type GenomicAlteration =
            { GeneName: string
              GenomicSource: string option
              Interpretation: string option
              ResultGroup: ResultGroup
              MolecularConsequence: string option
              TranscriptAlterationDetails: TranscriptAlterationDetails option
              HGVS: HGVS
              AlleleFrequency: string option }
        and TranscriptAlterationDetails =
            { StartPosition: string
              StopPosition: string
              Identifier: string
              ObservedNucleotide: string
              ReferenceNucelotide: string  }
        and ResultGroup =
            { Group: string
              Result: string }

        module GenomicAlteration =
            module Source =
                open type GenomicAlteration.Source
                open Utilities

                /// Validate that genomic alteration source is somatic
                let validate input =
                    match input with
                    | "Somatic" -> Ok Somatic
                    | _ -> Error $"Invalid genomic alteration source: {input}"

                /// Validate that the source of the genomic alteration, if it exists, is somatic.
                let validateOptional = Optional.validateWith validate

            module ResultGroup =
                open type GenomicAlteration.ResultGroup
                open type GenomicAlteration.MutatedResults
                open type GenomicAlteration.NoResults
                open type GenomicAlteration.NormalResults

                /// Validate that a result group is 'mutated', 'no result', or 'normal' and that the result names are valid.
                let validate resultGroup =
                    match (resultGroup.Group, resultGroup.Result) with
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
                    | _ -> Error $"Result - Invalid group and name: {resultGroup}"

            module AlleleFrequency =
                open Utilities
                open Utilities.StringValidations.Typed
                open type GenomicAlteration.AlleleFrequency

                let validate (input: string) =
                    input |> validateUnsignedInteger AlleleFrequency $"Invalid allele frequency: {input}"

                /// Validate that an allele frequency, if it exists, is a positive integer.
                let validateOptional =
                    Optional.validateWith validate

            module MolecularConsequence =
                open Utilities
                open type GenomicAlteration.MolecularConsequence

                /// Validate that the input for a molecular consequence is one of the following:
                /// 1. Blank
                /// 2. Missense
                /// 3. Nonsense
                /// 4. Frameshift
                /// 5. Noncoding
                /// 6. Codon deletion / insertion
                /// 7. Promoter
                let validate input =
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
                let validateOptional =
                    Optional.validateWith validate

            module TranscriptAlterationDetails =
                module StartPosition =
                    open Utilities.StringValidations.Typed
                    open type GenomicAlteration.Transcript.StartPosition

                    /// Validate that a start position is a valid unsigned integer
                    let validate input =
                        input |> validateUnsignedInteger64 StartPosition $"Invalid start position: {input}"


                module StopPosition =
                    open Utilities.StringValidations.Typed
                    open type GenomicAlteration.Transcript.StopPosition

                    /// Validate that a stop position is a valid unsigned integer
                    let validate input =
                        input |> validateUnsignedInteger64 StopPosition $"Invalid stop position: {input}"

                module TranscriptId =
                    open System.Text.RegularExpressions
                    open GenomicAlteration

                    /// Validate that a transcript id is either blank or starts with "NM_" followed by at least one digit.
                    let validate input =
                        if Regex("^(NM_\d{1,})*$").Match(input).Success then
                            Ok <| Transcript.Identifier input
                        else
                            Error $"Invalid transcript id: {input}"

                open FsToolkit.ErrorHandling

                /// Validate input for transcript alteration detail
                let validate transcriptAlterationDetails =
                    validation {
                        let! startPosition = transcriptAlterationDetails.StartPosition |> StartPosition.validate
                        and! stopPosition = transcriptAlterationDetails.StopPosition |> StopPosition.validate
                        and! transcriptId = transcriptAlterationDetails.Identifier |> TranscriptId.validate
                        and! observedNucleotide = transcriptAlterationDetails.ObservedNucleotide |> Nucleotide.validate
                        and! referenceNucleotide = transcriptAlterationDetails.ReferenceNucelotide |> Nucleotide.validate

                        let source = GenomicAlteration.Transcript.IdSource "RefSeq"

                        return ({
                            ObservedNucleotide = observedNucleotide
                            ReferenceNucleotide = referenceNucleotide
                            StartPosition = startPosition
                            StopPosition = stopPosition
                            Identifier = transcriptId
                            IdSource = source
                        } : GenomicAlteration.TranscriptAlterationDetails)
                    }

                open Utilities

                /// Validate optional input for transcript alteration detail
                let validateOptional = Optional.validateWith validate

            open FsToolkit.ErrorHandling

            /// Validate that genomic alteration has a valid gene name, result group, interpreation, molecular consequence, allele frequency, transcript alteration, hgvs, and source
            let validate (genomicAlteration: GenomicAlteration) =
                validation {
                    let! geneName = genomicAlteration.GeneName |> Gene.Name.validate
                    and! resultGroup = genomicAlteration.ResultGroup |> ResultGroup.validate
                    and! interpretation = genomicAlteration.Interpretation |> Gene.Interpretation.validateOptional
                    and! molecularConsequence = genomicAlteration.MolecularConsequence |> MolecularConsequence.validateOptional
                    and! alleleFrequency = genomicAlteration.AlleleFrequency |> AlleleFrequency.validateOptional
                    and! transcriptAlterationDetails = genomicAlteration.TranscriptAlterationDetails |> TranscriptAlterationDetails.validateOptional
                    and! hgvs = genomicAlteration.HGVS |> HGVS.validate
                    and! genomicSource = genomicAlteration.GenomicSource |> Source.validateOptional

                    return ({
                        GeneName = geneName
                        GenomicSource = genomicSource
                        ResultGroup = resultGroup
                        Interpretation = interpretation
                        AlleleFrequency = alleleFrequency
                        HGVS = hgvs
                        TranscriptAlterationDetails = transcriptAlterationDetails
                        MolecularConsequence = molecularConsequence
                    } : Domain.GenomicAlteration)
                }

        module GenomicAlterations =
            open Utilities

            /// Validate genomic alteration inputs
            let validate =
                Seq.map GenomicAlteration.validate
                >> Seq.toList
                >> Result.combine
                >> Result.mapError List.flatten

        type FusionResult =
            { Input: string
              Group: string  }

        type Fusion =
            { Gene1Name: string
              Gene2Name: string
              Exon1: string
              Exon2: string
              Interpretation: string
              Result: FusionResult }

        module Fusion =
            module Exon =
                open Utilities.StringValidations.Typed

                let validate input =
                    input |> validateUnsignedInteger Fusion.Exon $"Fusion exon not an unsigned integer: {input}"

            module Interpretation =
                open Utilities.StringValidations.Typed
                open type Fusion.Interpretation

                /// Validate that a fusion interpreation is not blank
                let validate = validateNotBlank Interpretation "Fusion interpretation can't be blank"

            module Result =
                open type Fusion.Result

                /// Validate that a fusion result is either pathogenic or is at lest detected
                let validate (fusionResult: FusionResult) =
                    match fusionResult.Input, fusionResult.Group with
                    | "Pathogenic Fusion", "Mutated" ->  Ok ``Pathogenic Fusion``
                    | "Fusion Detected", "Mutated" -> Ok ``Fusion Detected``
                    | _ -> Error $"Invalid fusion result: {fusionResult}"

            open FsToolkit.ErrorHandling

            /// Validate that a fusion has valid gene names, exons, interepretation, and result
            let validate fusion =
                validation {
                    let! gene1Name = fusion.Gene1Name |> Gene.Name.validate
                    and! gene2Name = fusion.Gene2Name |> Gene.Name.validate
                    and! exon1 = fusion.Exon1 |> Exon.validate
                    and! exon2 = fusion.Exon2 |> Exon.validate
                    and! interpretation = fusion.Interpretation |> Interpretation.validate
                    and! fusionResult =  fusion.Result |> Result.validate

                    return ({
                        Gene1Name = gene1Name
                        Gene2Name = gene2Name
                        Exon1 = exon1
                        Exon2 = exon2
                        Interpretation = interpretation
                        Result = fusionResult
                    } : Domain.Fusion)
                }

        module Fusions =
            open Utilities

            /// Validate a list of fusions
            let validate =
                List.map Fusion.validate
                >> Result.combine
                >> Result.mapError List.flatten

        type TumorMutationBurden =
            { Score: string
              Call: string }

        module TumorMutationBurden =
            module Score =
                open System.Text.RegularExpressions

                /// Attempt to convert a score input to a valid tmb score. If the regex is valid, convert it to an integer
                /// with the unit of measure: mutation/megabase.
                ///
                ///     (|ValidScore|_) (Input "3 per Mb") = Some (3 <mutation/megabase>)
                let (|ValidScore|_|)  input =
                    let m = Regex("^(?<score>\d{1,}) per Mb$").Match(input)

                    if m.Success
                    then Some (m.Groups.[1].Value |> int |> ((*) 1<mutation/megabase>))
                    else None

            open type TumorMutationBurden.Score

            /// Validate that a tumor mutation burden's call is indeterrminate, low, intermediate, or high and that, if applicable, the score is a valid score
            let validate (tumorMutationBurden: TumorMutationBurden)=
                match tumorMutationBurden.Call, tumorMutationBurden.Score with
                | "Indeterminate", "" -> Ok Indeterminate
                | "Low", Score.ValidScore tmbScore -> Ok <| Low tmbScore
                | "Intermediate", Score.ValidScore tmbScore -> Ok <| Intermediate tmbScore
                | "High", Score.ValidScore tmbScore -> Ok <| High tmbScore
                | _ -> Error $"Invalid tumor mutation burden: {tumorMutationBurden}"

            open Utilities
            /// Validate an optional Tumor Mutation Burden. If no input exists, it's valid.
            /// If an input exists, run validation checks on it.
            let validateOptional = Optional.validateWith validate

        type MicrosatelliteInstability =
            { Call: string
              ResultGroup: string }

        module MicrosatelliteInstability =
            open type MicrosatelliteInstability.Status

            /// Validate that a miscrosatellite instability is either stable, high, or indeterminate
            let validate msi =
                match msi.Call, msi.ResultGroup with
                | "Stable", "Normal" -> Ok Stable
                | "High", "High" -> Ok High
                | "Indeterminate", "No Result" -> Ok Indeterminate
                | _ -> Error $"Invalid microsatellite instability: {msi}"

            open Utilities

            /// Validate a microsatellite instability, if it exists
            let validateOptional = Optional.validateWith validate


        type Report =
            { Test: Test
              Patient: Patient
              OrderingMd: OrderingMd
              Pathologist: Pathologist
              Diagnosis: Diagnosis
              GenomicAlterations: GenomicAlteration seq
              Fusions: Fusion list
              Specimen: TumorSpecimen
              TumorMutationBurden: TumorMutationBurden option
              MicrosatelliteInstability: MicrosatelliteInstability option }

        module Report =
            open FsToolkit.ErrorHandling

            /// Validate that a report has a valid patient, ordering md, pathaolgist, genomic alterations, fusions, specimen, test, diagnosis, tumor mutaiton burden, and microsatellite instability status.
            let validate (report: Report) =
                validation {
                    let! patient = Patient.validate report.Patient
                    and! orderingMd = OrderingMd.validate report.OrderingMd
                    and! pathologistInfo = Pathologist.validate report.Pathologist
                    and! genomicAlterations = GenomicAlterations.validate report.GenomicAlterations
                    and! fusions = Fusions.validate report.Fusions
                    and! specimen = TumorSpecimen.validate report.Specimen
                    and! test = Test.validate report.Test
                    and! diagnosis = Diagnosis.validate report.Diagnosis
                    and! tmb = TumorMutationBurden.validateOptional report.TumorMutationBurden
                    and! msi = MicrosatelliteInstability.validateOptional report.MicrosatelliteInstability

                    return ({
                        Test = test
                        Patient = patient
                        OrderingMd = orderingMd
                        Pathologist = pathologistInfo
                        GenomicAlterations = genomicAlterations
                        Fusions = fusions
                        Specimen = specimen
                        Diagnosis = diagnosis
                        TumorMutationBurden = tmb
                        MicrosatelliteInstability = msi
                    } : Domain.Report)
                } |> Result.mapError (fun errors ->
                    ({ ReportId = report.Test.ReportId
                       Errors = errors } : Report.ValidationError))



    module Xml =
        open FSharp.Data
        open System.IO
        open System.Text.RegularExpressions
        open Utilities
        open Core.Input
        open Input

        [<Literal>]
        let CarisReportXsdPath = __SOURCE_DIRECTORY__ + "/data/carisReport.xsd"

        type ReportProvider = XmlProvider<Schema=CarisReportXsdPath, EmbeddedResource="OSTOR.ClinicalTrials.Reports, OSTOR.ClinicalTrials.Reports.carisReport.xsd">

        /// A Caris Report XML file
        type Report(filePath: string) =
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
                |> Option.map (fun tmb ->
                    { Call = tmb.MutationBurdenCall
                      Score = tmb.MutationBurdenScore
                    })

            member _.MicrosatelliteInstability =
                testResults
                |> Seq.tryPick (fun testResult -> testResult.MicrosatelliteInstability)
                |> Option.map (fun msi ->
                    { Call = msi.MsiCall
                      ResultGroup = msi.ResultGroup
                    })

            member _.RawGenomicAlterations =
                testResults
                |> Seq.choose (fun testResult -> testResult.GenomicAlteration)
                |> Seq.filter (fun ga -> Seq.head ga.Results |> fun result -> result <> "variantnotdetected")
                |> Seq.map (fun ga ->
                    {| BiomarkerName = ga.BiomarkerNames |> Seq.head
                       GeneName = ga.Genes |> Seq.head
                       Result = ga.Results |> Seq.head
                       ResultGroup = ga.ResultGroups |> Seq.head
                       HgvsCodingChange = ga.HgvsCodingChanges |> Seq.tryHead
                       HgvsProteinChange = ga.HgvsProteinChanges |> Seq.tryHead
                       Chromosome = ga.Chromosomes |> Seq.tryHead
                       Exon = ga.Exons |> Seq.tryHead
                       GenomicSource = ga.GenomicSources |> Seq.tryHead
                       Interpretation = ga.Interpretations |> Seq.tryHead
                       AlleleFrequency = ga.AlleleFrequencyInformations |> Seq.tryHead |> Option.map (fun afi -> afi.AlleleFrequency)
                       MolecularConsequence = ga.MolecularConsequences |> Seq.tryHead
                       TranscriptAlterationDetails = ga.AlterationDetails
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

            member this.GenomicAlterations : GenomicAlteration seq =
                this.RawGenomicAlterations
                |> Seq.map (fun ga ->
                    { GeneName = ga.GeneName
                      ResultGroup = { Group = ga.ResultGroup; Result = ga.Result }
                      Interpretation = ga.Interpretation
                      AlleleFrequency = ga.AlleleFrequency
                      GenomicSource = ga.GenomicSource
                      MolecularConsequence = ga.MolecularConsequence
                      HGVS = { CodingChange = ga.HgvsCodingChange; ProteinChange = ga.HgvsProteinChange }
                      TranscriptAlterationDetails = ga.TranscriptAlterationDetails |> Option.map (fun tad ->
                           { StartPosition = tad.TranscriptStartPosition
                             StopPosition = tad.TranscriptStopPosition
                             Identifier = tad.TranscriptId
                             ObservedNucleotide = tad.ObservedNucleotide
                             ReferenceNucelotide = tad.ReferenceNucleotide
                           })
                    })

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

            member _.Test: Test =
                { LabName = testDetails.LabName
                  ReportId = testDetails.LabReportId
                  OrderedDate = testDetails.OrderedDate
                  ReceivedDate = testDetails.ReceivedDate }

            member _.Patient : Patient =
                { LastName = patientInfo.LastName
                  FirstName = patientInfo.FirstName
                  DateOfBirth = patientInfo.Dob
                  Sex = patientInfo.Gender
                  MRN = patientInfo.Mrn }

            member this.OrderingMd : OrderingMd =
                let fullName : Person.FullName =
                    { LastName = this.PhysicianInformation.LastName
                      FirstName = this.PhysicianInformation.FirstName }

                let npi = this.PhysicianInformation.Npi

                { FullName = fullName
                  NationalProviderId = npi }

            member this.Pathologist : Pathologist =
                { Organization = this.PathologistInformation.Organization }

            member _.Diagnosis : Diagnosis =
                { Name = patientInfo.Diagnosis
                  Codes = patientInfo.IcdCode
                  PathologicDiagnosis = patientInfo.PathologicDiagnosis
                  Site = patientInfo.PrimarySite
                  Lineage = patientInfo.Lineage
                  SubLineage = patientInfo.SubLineage }

            member this.TumorSpecimen : TumorSpecimen =
                { SpecimenId = this.TumorSpecimenInfo.SpecimenId
                  AccessionId = this.TumorSpecimenInfo.SpecimenAccessionId
                  Type = this.TumorSpecimenInfo.SpecimenType
                  Site = this.TumorSpecimenInfo.SpecimenSite
                  CollectionDate = this.TumorSpecimenInfo.SpecimenCollectionDate
                  ReceivedDate = this.TumorSpecimenInfo.SpecimenReceivedDate
                }

            member this.Fusions : Fusion list =
                this.FusionTranslocations
                |> Seq.toList
                |> List.map (fun ft ->
                    { Gene1Name = ft.Gene1
                      Gene2Name = ft.Gene2
                      Exon1 = ft.Exon1
                      Exon2 = ft.Exon2
                      Interpretation = ft.Interpretation
                      Result = { Input = ft.Result; Group = ft.ResultGroup }
                    })

            /// The overall report input that encapsulates all the other inputs
            member this.Report =
                { Test = this.Test
                  Patient = this.Patient
                  OrderingMd = this.OrderingMd
                  Pathologist = this.Pathologist
                  Diagnosis = this.Diagnosis
                  GenomicAlterations = this.GenomicAlterations
                  Fusions = this.Fusions
                  Specimen = this.TumorSpecimen
                  TumorMutationBurden = this.TumorMutationBurden
                  MicrosatelliteInstability = this.MicrosatelliteInstability }


    module DTO =
        open Database
        open Domain

        /// Convert report's patient information to a patient database row.
        let tryPatientRow (report: Report) =
            report.Patient.TryMrnValue |> Option.map (fun mrnValue ->
                let patient = report.Patient
                let row = context.Public.Patients.Create()

                row.Mrn         <- mrnValue
                row.FirstName   <- patient.FirstName.Value
                row.LastName    <- patient.LastName.Value
                row.DateOfBirth <- patient.DateOfBirth.Value

                row
            )

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
        let tryReportRow (report: Report) =
            report.Patient.TryMrnValue |> Option.map (fun mrnValue ->
                let row = context.Public.Reports.Create()
                let test = report.Test
                let patient = report.Patient
                let orderingMd = report.OrderingMd
                let pathologist = report.Pathologist
                let diagnosis = report.Diagnosis

                // overall report info
                row.VendorCliaNumber <- "03D1019490"
                row.ReportId   <- test.ReportId.Value
                row.PatientMrn <- mrnValue
                row.IssuedDate <- test.ReceivedDate.Value

                // ordering physician
                row.OrderingPhysician       <- orderingMd.Name.Value |> Some
                row.OrderingPhysicianNumber <- orderingMd.NationalProviderId.Value |> Some

                // pathologist - only organizations get listed in caris reports
                row.Pathologist <- pathologist.TryOrganizationValue

                // diagnosis - caris doesn't report diagnosis dates
                row.DiagnosisName       <- diagnosis.DiagnosisName.Value
                row.DiagnosisIcd10Codes <- diagnosis.DiagnosisCodes.Values |> List.toArray |> Some

                // biomarkers
                row.TumorMutationalBurden <- report.TumorMutationBurden |> Option.bind (fun tmb -> tmb.TryValue) |> Option.map float
                row.MsiStatus <- report.MicrosatelliteInstability |> Option.map (fun msi -> msi.Value)

                row
            )

        /// Each report lists a sample that may be referred to across reports. Therefore, samples are given their own table and listings of a sample in a report are givne their own table.
        ///
        /// Only tumor samplesa are listed in Caris reports.
        let toSampleRow (report: Report) =
            let specimen = report.Specimen
            let row = context.Public.Samples.Create()

            row.SampleId       <- specimen.SpecimenId.Value
            row.SampleType     <- specimen.Type.Value
            row.Category       <- "tumor"
            row.BiopsySite     <- specimen.Site.Value |> Some

            row

        /// The parent sample and report must exist in the datbase.
        let toSampleReportRow (report: Report) =
            let specimen = report.Specimen
            let test = report.Test
            let row = context.Public.SampleReports.Create()

            row.CollectionDate <- specimen.CollectionDate.Value |> Some
            row.ReceiptDate    <- specimen.ReceivedDate.Value
            row.ReportId       <- test.ReportId.Value

            row

        /// Convert all genomic alterations with molecular consequences to 'gene' database rows
        let toGeneRows (report: Report) =
            let geneNames   = report.GenomicAlterationsWithMolecularConsequence |> Seq.toList |> List.map (fun ga -> ga.GeneName)
            let fusionGenes = report.Fusions |> Seq.toList |> List.collect (fun fusion -> fusion.GeneNames)

            geneNames @ fusionGenes
            |> List.map (fun geneName ->
                let row = context.Public.Genes.Create()
                row.Name <- geneName.Value
                row
            )

        /// Convert a report's genomic alterations that have a 'molecular consequence' to 'Variant' database rows.
        /// Each genomic alteration with a "molecular consequence" will also have an hgvs coding change and protein change.
        ///
        /// Each variant row has a parent sample report and a parent gene. The parent sample report and gene must already exist in the database.
        let toVariantRows (sampleReportId: System.Guid) (report: Report) =


            report.GenomicAlterationsWithMolecularConsequence
            |> Seq.map (fun ga ->
                let row = context.Public.Variants.Create()

                // Associate the variant to the gene by its name
                row.GeneName       <- ga.GeneName.Value
                row.SampleReportId <- sampleReportId
                row.Name           <- ga.TryHgvsProteinChangeValue |> Option.defaultValue ""

                row.AllelicFraction <- ga.TryAlleleFrequencyValue |> Option.map float
                row.Category <- ga.SourceValue // is 'somatic'
                row.Type <- ga.MolecularConsequenceValue |> Some

                row.HgvsC <- ga.TryHgvsCodingChangeValue
                row.HgvsProtein <- ga.TryHgvsProteinChangeValue
                row.Transcript <- ga.TryTranscriptIdValue

                row)

        /// Assumes that fusion genes already exist in the database
        let toFusionRows (sampleReportId: System.Guid) (report: Report) =
            report.Fusions
            |> Seq.map (fun fusion ->
                let row = context.Public.Fusions.Create()

                row.SampleReportId <- sampleReportId
                // reow.SampleReportId
                row.FirstGeneName  <- fusion.Gene1Name.Value
                row.SecondGeneName <- fusion.Gene2Name.Value
                row.Description    <- fusion.Interpretation.Value |> Some
                row.FusionType <- "mutation"

                row
            )

        // Find the existing sample report based on this report's report id and sample id
        let querySampleReportId (reportId: ReportId) (specimenId: Specimen.Identifier) =
            query {
                for sampleReport in context.Public.SampleReports do
                where (sampleReport.ReportId = reportId.Value && sampleReport.SampleId = specimenId.Value)
                select sampleReport.Id
                exactlyOne
            }

        open Utilities

        let tryInsertRows (report: Report) =
            report.Patient.TryMrnValue |> Option.map (fun _ ->
                toVendorRow |> ignore
                report |> tryPatientRow |> Optional.value |> ignore
                report |> toGeneRows |> ignore
                report |> toSampleRow |> ignore

                context.SubmitUpdates()

                report |> tryReportRow  |> Optional.value |> ignore

                context.SubmitUpdates()

                report |> toSampleReportRow |> ignore

                context.SubmitUpdates()

                let sampleReportId = querySampleReportId report.Test.ReportId report.Specimen.SpecimenId

                report |> toFusionRows sampleReportId |> ignore
                report |> toVariantRows sampleReportId |> ignore
            )