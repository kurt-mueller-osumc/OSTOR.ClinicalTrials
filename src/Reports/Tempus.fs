namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    module Domain =
        open Core.Domain

        module Report =
            type Identifier =
                internal | Identifier of System.Guid
                member this.Value = this |> fun (Identifier id) -> id

            type Pathologist =
                internal | Pathologist of string
                member this.Value = this |> fun (Pathologist pathologist) -> pathologist

            type SignoutDate =
                internal | SignoutDate of System.DateTime
                member this.Value = this |> fun (SignoutDate signoutDate) -> signoutDate

        /// the `report` section of the Tempus report
        type Report =
            { ReportId: Report.Identifier
              SigningPathologist: Report.Pathologist
              SignoutDate: Report.SignoutDate }


        module HGVS =
            /// An abbreviated HGVS protein change
            type AbbreviatedProteinChange =
                internal | AbbreviatedProteinChange of string
                member this.Value = this |> fun (AbbreviatedProteinChange abbreviatedChange) -> abbreviatedChange

            /// a full hgvs protein change
            type FullProteinChange =
                internal | FullProteinChange of string
                member this.Value = this |> fun (FullProteinChange fullChange) -> fullChange

            type ProteinChange =
                { Abbreviated: AbbreviatedProteinChange
                  Full: FullProteinChange }

            type CodingChange =
                internal | CodingChange of string
                member this.Value = this |> fun (CodingChange codingChange) -> codingChange

            type ReferenceSequence =
                internal | ReferenceSequence of string
                member this.Value = this |> fun (ReferenceSequence referenceSequence) -> referenceSequence

            /// Human Genome Variation Society Sequence Variant
            type Variant =
                { ProteinChange: ProteinChange option
                  CodingChange: CodingChange
                  ReferenceSequence: ReferenceSequence }

        type DiagnosisName = internal | DiagnosisName of string
        type DiagnosisDate = internal | DiagnosisDate of System.DateTime

        module Patient =
            type TempusIdentifier =
                internal | TempusIdentifier of System.Guid
                member this.Value = this |> fun (TempusIdentifier tempusId) -> tempusId

            type Sex =
                internal | Male | Female
                member this.Value =
                    match this with
                    | Male -> "male"
                    | Female -> "female"


        /// the `patients` section in the Tempus report
        type Patient =
            { MRN: Patient.MRN option
              TempusId: Patient.TempusIdentifier
              FirstName: Person.FirstName
              LastName: Person.LastName
              DateOfBirth: Patient.BirthDate
              Sex: Patient.Sex
              DiagnosisName: DiagnosisName
              DiagnosisDate: DiagnosisDate option }


        module Order =
            type Identifier  = internal Identifier of string
            type OrderAccessionId = internal OrderAccessionId of string
            type Physician = internal Physician of string // ordering md
            type Institution = internal Institution of string

            /// The `test` subsection of the `order` section in the Tempus report
            type Test =
                { TestCode: TestCode
                  TestName: TestName
                  TestDescription: TestDescription }
            and TestCode = internal TestCode of string
            and TestName = internal TestName of string
            and TestDescription = internal TestDescription of string

        /// The `order` section of the Tempus report.
        type Order =
            { OrderingInstitution: Order.Institution
              OrderingPhysician: Order.Physician
              OrderId: Order.Identifier
              AccessionId: Order.OrderAccessionId
              Test: Order.Test }


        module Sample =
            type Identifier = internal Identifier of System.Guid
            type Site = internal Site of string
            type Type =
                internal
                | Blood | ``FFPE Block`` | ``FFPE Slides (Unstained)`` | Saliva

            type Dates =
                { CollectionDate: CollectionDate
                  ReceivedDate: ReceivedDate }
            and CollectionDate = internal CollectionDate of System.DateTime
            and ReceivedDate = internal ReceivedDate of System.DateTime

            type BlockId = BlockId of string
            type TumorPercentage = internal TumorPercentage of uint

        /// the tumor sample that will be present in the `specimens` section of the report
        type TumorSample =
            { SampleId: Sample.Identifier
              Site: Sample.Site
              Type: Sample.Type
              Dates: Sample.Dates
              BlockId: Sample.BlockId option
              TumorPercentage: Sample.TumorPercentage option }

        /// the normal sample that might or might not exist in the `specimens` section of the report
        type NormalSample =
            { SampleId: Sample.Identifier
              Site: Sample.Site
              Type: Sample.Type
              Dates: Sample.Dates
              BlockId: Sample.BlockId option }

        type Gene =
            { Name: Gene.Name
              HgncId: Gene.HgncId
              EntrezId: Gene.EntrezId }

        module Variant =
            type NucleotideAlteration =
                internal | NucleotideAlteration of string
                member this.Value = this |> fun (NucleotideAlteration na) -> na

            type AllelicFraction =
                internal | AllelicFraction of float
                member this.Value = this |> fun (AllelicFraction allelicFraction) -> allelicFraction

            type Description = internal | Description of string


        module Fusion =
            type Type = internal | GeneFusion
            type VariantDescription =
                internal
                | ``Chromosomal rearrangement``
                | ``Deletion (exons 2-7)``

        type Fusion =
            { ``5' Gene``: Gene
              ``3' Gene``: Gene
              FusionType: Fusion.Type option
              VariantDescription: Fusion.VariantDescription }

            member this.Genes = [this.``5' Gene``; this.``3' Gene``]


        module TumorMutationBurden =
            type Score =
                internal | Score of float
                with member this.Value = this |> fun (Score score) -> score

            and Percentile =
                internal | Percentile of uint
                with member this.Value = this |> fun (Percentile percentile) -> percentile

        /// tumor mutation burden found in the `results` section of the report
        type TumorMutationBurden =
            { Score: TumorMutationBurden.Score
              Percentile: TumorMutationBurden.Percentile }

        /// the microsattellite instability status found in the `results` section of the report
        type MicrosatelliteInstabilityStatus =
            internal | MicrosatelliteInstabilityStatus of string
            member this.Value = this |> fun (MicrosatelliteInstabilityStatus msiStatus) -> msiStatus

        type VariantCall = internal | SomaticVariant | GermlineVariant
        type SomaticVariantJudgement =
            internal
            | PotentiallyActionable
            | BiologicallyRelevant
            | UnknownSignificance

        module SomaticPotentiallyActionable =
            type Variant =
                { HGVS: HGVS.Variant
                  NucleotideAlteration: Variant.NucleotideAlteration option
                  AllelicFraction: Variant.AllelicFraction option
                  Description: Variant.Description }

            /// represents a mutation found in `results.somaticPotentiallyActionableMutations` section of the report
            type Mutation =
                { Gene: Gene
                  Variants: Variant list }

            module CopyNumberVariant =
                type Description =
                    internal
                    | ``Copy number gain``
                    | ``Copy number loss``

                    member this.Value =
                        match this with
                        | ``Copy number gain`` -> "copy number gain"
                        | ``Copy number loss`` -> "copy number loss"

                type Type =
                    internal
                    | Amplification
                    | Deletion

                    member this.Value =
                        match this with
                        | Amplification -> "amplification"
                        | Deletion -> "deletion"

            type CopyNumberVariant =
                { Gene: Gene
                  Description: CopyNumberVariant.Description
                  Type: CopyNumberVariant.Type
                }

        module SomaticBiologicallyRelevant =
            type Mutation =
                internal
                | RelevantGene of Gene | RelevantFusion of Fusion

                member this.Genes =
                    match this with
                    | RelevantGene gene -> [gene]
                    | RelevantFusion fusion -> fusion.Genes

            type Type =
                internal
                | CNV
                | SNV
                | Fusion

            type Variant =
                { Mutation: Mutation
                  HGVS: HGVS.Variant option
                  AllelicFraction: Variant.AllelicFraction option
                  NucleotideAlteration: Variant.NucleotideAlteration option
                  Type: Type }

        module SomaticVUS =
            type Type = internal | SNV

        /// an entry found in `results.SomaticVariantsOfUnknownSignificance`
        type SomaticVUS =
            { Gene: Gene
              Hgvs: HGVS.Variant
              Type: SomaticVUS.Type
              Description: Variant.Description
              NucleotideAlteration: Variant.NucleotideAlteration
              AllelicFraction: Variant.AllelicFraction
            }

        module InheritedVariants =
            type Note = internal Note of string
            type Disease = internal Disease of string
            type Chromosome = internal Chromosome of uint
            type ReferencedNucleotide = ReferencedNucleotide of string
            type AlteredNucleotide = AlteredNucleotide of string
            type Position = Position of uint

            type Value<'ClinicalSignificance> =
                { Gene: Gene
                  HGVS: HGVS.Variant
                  Description: Variant.Description
                  ClinicalSignificance: 'ClinicalSignificance
                  Disease: Disease
                  AllelicFraction: Variant.AllelicFraction
                  Chromosome: Chromosome
                  ReferencedNucleotide: ReferencedNucleotide
                  Position: Position
                  AlteredNucleotide: AlteredNucleotide }

        type InheritedVariants<'ClinicalSignificance> =
            { Note: InheritedVariants.Note option
              Values: InheritedVariants.Value<'ClinicalSignificance> list }

        module InheritedRelevantVariants =
            type ClinicalSignificance =
                internal
                | ``Likely Pathogenic``
                | Pathogenic
                | ``Risk Allele``
                | ``VUS Favoring Pathogenic``

        module InheritedVUS =
            type ClinicalSignificance =
                internal | ``Variant of Unknown Significance``

        type InheritedRelevantVariants = InheritedVariants<InheritedRelevantVariants.ClinicalSignificance>
        type InheritedVUS = InheritedVariants<InheritedVUS.ClinicalSignificance>

        /// represents the `results` section of the Tempus report
        type Results =
            { TumorMutationBurden: TumorMutationBurden option
              MicrosatelliteInstabilityStatus: MicrosatelliteInstabilityStatus option
              ``Somatic Potentially Actionable Mutations``: SomaticPotentiallyActionable.Mutation list
              ``Somatic Potentially Actionable Copy Number Variants``: SomaticPotentiallyActionable.CopyNumberVariant list
              ``Somatic Biologically Relevant Variants``: SomaticBiologicallyRelevant.Variant list
              ``Somatic Variants of Unknown Significance``: SomaticVUS list
              Fusions: Fusion list
              ``Inherited Relevant Variants``: InheritedRelevantVariants
              ``Inherited Variants of Unknown Significance``: InheritedVUS
            }

        type OverallReport =
            { Lab: Lab
              Patient: Patient
              Report: Report
              Order: Order
              TumorSample: TumorSample
              NormalSample: NormalSample option
              Results: Results
            }

    module Json =
        open Thoth.Json.Net
        open Utilities

        type Lab =
            { Name: string
              StreetAddress: string
              City: string
              State: string
              Zip: string
              CliaNumber: string }

            static member CamelCaseDecoder : Decoder<Lab> =
                Decode.object (fun get ->
                    { Name          = "name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "streetAddress" |> flip get.Required.Field Decode.string
                      City          = "city"          |> flip get.Required.Field Decode.string
                      State         = "state"         |> flip get.Required.Field Decode.string
                      Zip           = "zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "cliaNo"        |> flip get.Required.Field Decode.string }
                )

            static member PascalCaseDecoder : Decoder<Lab> =
                Decode.object (fun get ->
                    { Name          = "Name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "StreetAddress" |> flip get.Required.Field Decode.string
                      City          = "City"          |> flip get.Required.Field Decode.string
                      State         = "State"         |> flip get.Required.Field Decode.string
                      Zip           = "Zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "clia_no"       |> flip get.Required.Field Decode.string }
                )

            /// Deserializer for the `lab` json object. Object attributes can be camel-cased or pascal-cased, save
            /// for the lab's clia #, which is camel-cased or snake-cased for some reason.
            static member Decoder : Decoder<Lab> =
                Decode.oneOf [ Lab.CamelCaseDecoder; Lab.PascalCaseDecoder ]

        module Lab =
            open FsToolkit.ErrorHandling

            open StringValidations
            open Core
            open Core.Input

            /// Validate the `lab` section of the json report
            let validate (json: Lab) : Validation<Domain.Lab,string> =
                validation {
                    let! labName = json.Name |> Lab.Name.validate
                    and! cliaNumber = json.CliaNumber |> Lab.CliaNumber.validate
                    and! streetAddress = json.StreetAddress |> StreetAddress.validate
                    and! city = json.City |> validateNotBlank |> Result.map Domain.City
                    and! state = json.State |> validateNotBlank |> Result.map Domain.State
                    and! zip = json.Zip |> validateNotBlank |> Result.map Domain.ZipCode

                    return ({ Name = labName
                              Address = {
                                  Street = streetAddress
                                  City = city
                                  State = state
                                  Zip = zip
                              }
                              CliaNumber = cliaNumber
                            } : Domain.Lab)
                }


        type Patient =
            { FirstName: string
              LastName: string
              TempusId: System.Guid
              Mrn: string option
              Sex: string
              DateOfBirth: System.DateTime
              Diagnosis: string
              DiagnosisDate: System.DateTime option }

            /// Deserializer for the 'patient' json object.
            ///
            /// The following object attributes can be camel-cased or snake-cased:
            /// - `emrId` / `emr_id`
            /// - `dateOfBirth` / `DoB`
            static member Decoder : Decoder<Patient> =
                Decode.object (fun get ->
                    let diagnosisDate = "diagnosisDate" |> flip get.Required.Field Decoder.Optional.dateTime
                    let mrnDecoder = ["emrId"; "emr_id"] |> List.map (flip Decode.field Decoder.Optional.string) |> Decode.oneOf
                    let dobDecoder = ["dateOfBirth"; "DoB"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      Mrn           = get.Required.Raw mrnDecoder
                      Sex           = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = get.Required.Raw dobDecoder
                      Diagnosis     = "diagnosis"   |> flip get.Required.Field Decode.string
                      DiagnosisDate = diagnosisDate }
                )


        module Patient =

            module Diagnosis =
                open Utilities.StringValidations.Typed

                module Name =
                    /// Validate that diagnosis name is not blank
                    let validate = validateNotBlank Domain.DiagnosisName "Diagnosis name can't be blank"

            module Sex =
                open Domain.Patient

                /// Validate that sex is either `"(M|m)ale"` or `"(F|f)emale"`
                let validate (str: string) : Result<Sex, string> =
                    match str with
                    | "Male" | "male" -> Ok Male
                    | "Female" | "female" -> Ok Female
                    | _ -> Error $"Invalid sex: {str}"

            open FsToolkit.ErrorHandling
            open Core

            /// Validate a patient input: their name, mrn, and sex
            let validate (patient: Patient) : Validation<Domain.Patient,string> =
                validation {
                    let! firstName = patient.FirstName |> Input.Person.FirstName.validate
                    and! lastName  = patient.LastName  |> Input.Person.LastName.validate
                    and! mrn       = patient.Mrn |> Input.Patient.MRN.validateOptional
                    and! sex       = patient.Sex |> Sex.validate
                    and! diagnosisName = patient.Diagnosis |> Diagnosis.Name.validate

                    let tempusId  = patient.TempusId |> Domain.Patient.TempusIdentifier
                    let birthDate = patient.DateOfBirth |> Domain.Patient.BirthDate
                    let diagnosisDate = patient.DiagnosisDate |> Option.map Domain.DiagnosisDate

                    return ({ MRN = mrn
                              FirstName = firstName
                              LastName  = lastName
                              TempusId  = tempusId
                              Sex = sex
                              DateOfBirth   = birthDate
                              DiagnosisName = diagnosisName
                              DiagnosisDate = diagnosisDate
                             } : Domain.Patient)
                }


        type Report =
            { ReportId: System.Guid
              SigningPathologist: string
              SignoutDate: System.DateTime }

            /// Deserializer for the "report" section of the Tempus report
            static member Decoder : Decoder<Report> =
                Decode.object (fun get ->
                    let pathologistDecoder = ["signing_pathologist"; "signingPathologist"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf
                    let signoutDateDecoder = ["signout_date"; "signoutDate"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { ReportId           = "reportId"         |> flip get.Required.Field Decode.guid
                      SigningPathologist = pathologistDecoder |> get.Required.Raw
                      SignoutDate        = signoutDateDecoder |> get.Required.Raw
                    } )

        module Report =
            open FsToolkit.ErrorHandling
            open Domain.Report

            module Pathologist =
                open Utilities.StringValidations.Typed

                /// Validate that a signing pathologist's name is not blank
                let validate = validateNotBlank Pathologist "Report's signing pathologist cannot be blank"

            /// Validate `report` section of the Tempus report
            let validate (report: Report) : Validation<Domain.Report,string> =
                validation {
                    let! signingPathologist = report.SigningPathologist |> Pathologist.validate

                    let reportId    = Identifier report.ReportId
                    let signoutDate = SignoutDate report.SignoutDate

                    return ({ ReportId = reportId
                              SigningPathologist = signingPathologist
                              SignoutDate = signoutDate
                            } : Domain.Report)
                }


        /// Json object attributes that identifies genes
        type Gene =
            { GeneId: string // 'gene' attribute
              HgncId: string
              EntrezId: string }
            /// Deserializes a gene's json object attributes
            static member Decoder : Decoder<Gene> =
                Decode.object (fun get ->
                    { GeneId   = "gene"     |> flip get.Required.Field Decode.string
                      HgncId   = "hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 5 json object attributes
            static member Gene5Decoder : Decoder<Gene> =
                Decode.object (fun get ->
                    { GeneId   = "gene5"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene5hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene5entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 3 json object attributes
            static member Gene3Decoder : Decoder<Gene> =
                Decode.object (fun get ->
                    { GeneId   = "gene3"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene3hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene3entrezId" |> flip get.Required.Field Decode.string })

        module Gene =
            open Core.Domain
            open StringValidations

            /// Validate that a gen's json object representation has a gene name, hgnc id, and entrez id
            let validate (gene: Gene) : Result<Domain.Gene, string> =
                match (gene.GeneId, gene.HgncId, gene.EntrezId) with
                | (NotBlank, NotBlank, NotBlank) ->
                    Ok { Name = Gene.Name gene.GeneId
                         HgncId = Gene.HgncId gene.HgncId
                         EntrezId = Gene.EntrezId gene.EntrezId }
                | _ -> Error $"Gene missing name, hgnc id, or entrez id: {gene}"

        type Fusion =
            { Gene5: Gene
              Gene3: Gene
              VariantDescription: string
              FusionType: string option
              StructuralVariant: string option }

            /// Deserializer for a fusion of genes
            static member Decoder : Decoder<Fusion> =
                Decode.object (fun get ->
                    { Gene5 = get.Required.Raw Gene.Gene5Decoder
                      Gene3 = get.Required.Raw Gene.Gene3Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      FusionType         = "fusionType"         |> flip get.Required.Field Decoder.Optional.string
                      StructuralVariant  = "structuralVariant"  |> flip get.Required.Field Decoder.Optional.string }
                )

        module Fusion =
            module Type =
                /// Validate that a fusion type is `gene`, for now.
                let validate fusionType =
                    match fusionType with
                    | "gene" -> Ok Domain.Fusion.GeneFusion
                    | _ -> Error $"Invalid fusion type: {fusionType}"

                /// Validate an optional fusion type if it exists.
                let validateOptional = Optional.validateWith validate

            module VariantDescription =
                /// Validate that a fusion variant description is either "Chromosomal rearrangement" or "Deletion (exons 2-7)"
                let validate description =
                    match description with
                    | "Chromosomal rearrangement" -> Ok Domain.Fusion.``Chromosomal rearrangement``
                    | "Deletion (exons 2-7)" -> Ok Domain.Fusion.``Deletion (exons 2-7)``
                    | _ -> Error $"Invalid fusion variant description: {description}"

            open FsToolkit.ErrorHandling

            /// Validate that a fusion has 2 valid genes and a valid fusion type, if present.
            let validate (fusion: Fusion) : Validation<Domain.Fusion, string> =
                validation {
                    let! gene5 = fusion.Gene5 |> Gene.validate
                    and! gene3 = fusion.Gene3 |> Gene.validate
                    and! fusionType = fusion.FusionType |> Type.validateOptional
                    and! variantDescription = fusion.VariantDescription |> VariantDescription.validate

                    return ({ ``5' Gene`` = gene5
                              ``3' Gene`` = gene3
                              FusionType = fusionType
                              VariantDescription = variantDescription
                            } : Domain.Fusion)
                }

        module Fusions =
            /// Validate a list of fusions
            let validate (jsons: Fusion list) =
                jsons
                |> List.map Fusion.validate
                |> Result.combine
                |> Result.mapError List.flatten


        /// An `order` json object
        type Order =
            { Institution: string
              Physician: string
              OrderId: string
              AccessionId: string
              OrderTest: Test }

            /// Deserializer for the 'order' json object.
            ///
            ///  The following object attributes can be camel-cased or snake-cased:
            /// - `"tempusOrderId"` / `"tempusOrder_id"`
            static member Decoder: Decoder<Order> =
                Decode.object (fun get ->
                    let orderIdDecoder = ["tempusOrderId"; "tempusOrder_id"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf

                    { Institution = "institution"  |> flip get.Required.Field Decode.string
                      Physician   = "physician"    |> flip get.Required.Field Decode.string
                      OrderId     = orderIdDecoder |> get.Required.Raw
                      AccessionId = "accessionId"  |> flip get.Required.Field Decode.string
                      OrderTest   = "test"         |> flip get.Required.Field Test.Decoder }
                )

        and Test =
            { Code: string
              Name: string
              Description: string }

            /// Deserializer for the actual test code, name, and description ran on the report's patient
            static member Decoder : Decoder<Test> =
                Decode.object (fun get ->
                    { Code        = "code" |> flip get.Required.Field Decode.string
                      Name        = "name" |> flip get.Required.Field Decode.string
                      Description = "description" |> flip get.Required.Field Decode.string }
                )

        module Order =
            module Test =
                open Utilities.StringValidations.Typed
                open FsToolkit.ErrorHandling

                let validateCode = validateNotBlank Domain.Order.TestCode "Order test code can't be blank"
                let validateName = validateNotBlank Domain.Order.TestName "Order test name can't be blank"
                let validateDescription = validateNotBlank Domain.Order.TestDescription "Order test description can't be blank"

                /// Validate that a order test's code, name, and description are not blank
                let validate (test: Test) : Validation<Domain.Order.Test,string> =
                    validation {
                        let! testCode = test.Code |> validateCode
                        and! testName = test.Name |> validateName
                        and! testDescription = test.Description |> validateDescription

                        return ({
                            TestName = testName
                            TestCode = testCode
                            TestDescription = testDescription
                        } : Domain.Order.Test)
                    }

            module Identifier =
                open System.Text.RegularExpressions

                /// Validate that a tempus order id is in the following format where the first character is a 0, 1 or 2, followed by one digit, follwed by four letters: `"(0|1|2)dxxxx"`
                ///
                ///    validate "20hnyc" = Ok (Domain.Order.Identifier "20hnyc")
                ///    validate "30aaaa" = Error "Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: 30aaaa"
                let validate identifier : Result<Domain.Order.Identifier, string> =
                    if Regex("^(0|1|2){1}\d{1}[a-zA-z]{4}$").Match(identifier).Success then
                        Ok <| Domain.Order.Identifier identifier
                    else
                        Error $"Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: {identifier}"

            module AccessionId =
                open System.Text.RegularExpressions

                /// Validate that Tempus order's accession id is the in the following format where d is a digit and x is any alphanumeric character: `"TL-(0|1|2)d-xxxxxx"`
                ///
                ///    validate "TL-19-DF60D1" = Ok (Domain.Order.AccessionId "TL-19-DF60D1")
                //     validate "TL-33-AAAAAA" = Error "Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: TL-33-AAAAAA"
                let validate accessionId : Result<Domain.Order.OrderAccessionId, string> =
                    if Regex("^TL-(0|1|2){1}\d{1}-(\d|[A-Z]|[a-z]){6}$").Match(accessionId).Success then
                        Ok <| Domain.Order.OrderAccessionId accessionId
                    else
                        Error $"Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: {accessionId}"

            open Utilities.StringValidations
            open FsToolkit.ErrorHandling

            /// Validate the `order` section of the json report
            let validate (order: Order) : Validation<Domain.Order, string> =
                validation {
                    let! institution = order.Institution |> validateNotBlank |> Result.map Domain.Order.Institution
                    and! physician = order.Physician |> validateNotBlank |> Result.map Domain.Order.Physician
                    and! orderId = order.OrderId |> Identifier.validate
                    and! accessionId = order.AccessionId |> AccessionId.validate
                    and! orderTest = order.OrderTest |> Test.validate

                    return ({ OrderingInstitution = institution
                              OrderingPhysician = physician
                              OrderId = orderId
                              AccessionId = accessionId
                              Test = orderTest
                            } : Domain.Order)
                }

        /// each entry in the `specimens` section of the Tempus report
        type Sample =
            { SampleId: System.Guid
              CollectionDate: System.DateTime
              ReceivedDate: System.DateTime
              SampleCategory: string
              SampleSite: string
              SampleType: string
              Institution: Institution }

            /// Deserializer for the sample seciton of the Tempus report
            static member Decoder : Decoder<Sample> =
                Decode.object (fun get ->
                    { SampleId       = "tempusSampleId"  |> flip get.Required.Field Decode.guid
                      CollectionDate = "collectionDate"  |> flip get.Required.Field Decode.datetime
                      ReceivedDate   = "receiptDate"     |> flip get.Required.Field Decode.datetime
                      SampleCategory = "sampleCategory"  |> flip get.Required.Field Decode.string
                      SampleSite     = "sampleSite"      |> flip get.Required.Field Decode.string
                      SampleType     = "sampleType"      |> flip get.Required.Field Decode.string
                      Institution    = "institutionData" |> flip get.Required.Field Institution.Decoder })

        and Institution =
            { BlockId: string option
              TumorPercentage: uint option }

            static member Decoder : Decoder<Institution> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.uint32
                    } )

        module Sample =
            open Domain.Sample

            module Type =
                /// Validate that a sample type is either blood, block, slides, or saliva.
                let validate sampleType =
                    match sampleType with
                    | "Blood" -> Ok Blood
                    | "FFPE Block" -> Ok ``FFPE Block``
                    | "FFPE Slides (Unstained)" -> Ok ``FFPE Slides (Unstained)``
                    | "Saliva" -> Ok Saliva
                    | _ -> Error $"Invalid sample type: {sampleType}"

            module Site =
                open Utilities.StringValidations.Typed

                let validate = validateNotBlank Site "Sample site cannot be blank"

            type Dates = {
                CollectionDate: System.DateTime
                ReceivedDate: System.DateTime
            }

            module Dates =
                /// Validate that sample's collection date happens before its received date
                let validate dates : Result<Domain.Sample.Dates, string> =
                    if dates.CollectionDate < dates.ReceivedDate then
                        Ok ({ CollectionDate = CollectionDate dates.CollectionDate
                              ReceivedDate   = ReceivedDate dates.ReceivedDate
                            })
                    else
                        Error $"Collection date, {dates.CollectionDate}, doesn't occur before received date, {dates.ReceivedDate}"


        module TumorSample =
            module Category =
                /// Validate that a tumor sample category is 'tumor'
                let validate category =
                    match category with
                    | "tumor" -> Ok "tumor"
                    | _ -> Error $"Sample category is not tumor: {category}"

            open FsToolkit.ErrorHandling
            open Utilities.StringValidations
            open Domain.Sample

            /// Validate a tumor sample
            let validate (json: Sample) : Validation<Domain.TumorSample, string> =
                validation {
                    // validate that the sample's listed category is 'tumor'
                    let! tumorCategory = json.SampleCategory |> Category.validate
                    and! sampleDates   = { Sample.Dates.CollectionDate = json.CollectionDate
                                           Sample.Dates.ReceivedDate   = json.ReceivedDate } |> Sample.Dates.validate
                    and! sampleSite    = json.SampleSite |> Sample.Site.validate
                    and! sampleType    = json.SampleType |> Sample.Type.validate

                    let sampleId        = Identifier json.SampleId
                    let blockId         = json.Institution.BlockId |> Option.map BlockId
                    let tumorPercentage = json.Institution.TumorPercentage |> Option.map TumorPercentage

                    return ({ SampleId = sampleId
                              Site = sampleSite
                              Dates = sampleDates
                              Type = sampleType
                              BlockId = blockId
                              TumorPercentage = tumorPercentage
                            } : Domain.TumorSample)
                }

        module NormalSample =
            module Category =
                /// Validate that a normal sample category is 'normal'
                let validate category =
                    match category with
                    | "normal" -> Ok "normal"
                    | _ -> Error $"Sample category is not normal: {category}"

            open FsToolkit.ErrorHandling
            open Domain.Sample

            /// Validate a normal sample
            let validate (json: Sample) : Validation<Domain.NormalSample, string> =
                validation {
                    // validate that the sample's listed category is 'normal'
                    let! normalCategory = json.SampleCategory |> Category.validate
                    and! sampleDates    = { Sample.Dates.CollectionDate = json.CollectionDate
                                            Sample.Dates.ReceivedDate   = json.ReceivedDate } |> Sample.Dates.validate
                    and! sampleSite     = json.SampleSite |> Sample.Site.validate
                    and! sampleType     = json.SampleType |> Sample.Type.validate

                    return ({ SampleId = Identifier json.SampleId
                              Site = sampleSite
                              Dates = sampleDates
                              Type = sampleType
                              BlockId = json.Institution.BlockId |> Option.map BlockId
                             } : Domain.NormalSample)
                }

            /// Validate normal sample, if it's present
            let validateOptional  =
                Optional.validateWith validate


        /// Represents HGVS reference sequences for proteins and coding DNA
        type HGVS =
            { ``HGVS.p``: string // abbreviated protein sequence change
              ``HGVS.pFull``: string // full protein sequence change
              ``HGVS.c``: string // coding DNA sequence change
              Transcript: string // the reference sequence (eg NM_012345.1)
              MutationEffect: string // will either equal 'HGVS.p' or 'HGVS.c'
            }

            /// Deserializer for hgvs json object attributes
            static member Decoder : Decoder<HGVS> =
                Decode.object (fun get ->
                    { ``HGVS.p``     = "HGVS.p"         |> flip get.Required.Field Decode.string
                      ``HGVS.pFull`` = "HGVS.pFull"     |> flip get.Required.Field Decode.string
                      ``HGVS.c``     = "HGVS.c"         |> flip get.Required.Field Decode.string
                      Transcript     = "transcript"     |> flip get.Required.Field Decode.string
                      MutationEffect = "mutationEffect" |> flip get.Required.Field Decode.string }
                )

        module HGVS =
            open Utilities.StringValidations

            /// Validate a HGVS reference sequence:
            ///
            /// 1. all hgvs fields are blank OR
            /// 2. if hgvs.c is present and hgvs.p is blank, mutationEffect == hgvs.c OR
            /// 3. if hgvs.p is present
            ///    - hgvs.pFull (vice versa) is present
            ///    - hgvs.p == mutation effect
            let validate (hgvs: HGVS) : Result<Domain.HGVS.Variant, string> =
                match (hgvs.``HGVS.p``, hgvs.``HGVS.pFull``, hgvs.``HGVS.c``, hgvs.MutationEffect, hgvs.Transcript) with
                /// No HGVS protein sequence change present; only a coding DNA sequence is present
                | (BlankString, BlankString, NotBlank, NotBlank, NotBlank) when hgvs.``HGVS.c`` = hgvs.MutationEffect ->
                    Ok <| { ProteinChange = None
                            CodingChange = Domain.HGVS.CodingChange hgvs.``HGVS.c``
                            ReferenceSequence = Domain.HGVS.ReferenceSequence hgvs.Transcript }
                /// Both HGVS protein sequence change and coding DNA sequence change are present
                | (NotBlank, NotBlank, NotBlank, NotBlank, NotBlank) when hgvs.``HGVS.p`` = hgvs.MutationEffect ->
                    Ok <| { ProteinChange = Some { Abbreviated =  Domain.HGVS.AbbreviatedProteinChange hgvs.``HGVS.p``
                                                   Full = Domain.HGVS.FullProteinChange hgvs.``HGVS.pFull`` }
                            CodingChange = Domain.HGVS.CodingChange hgvs.``HGVS.c``
                            ReferenceSequence = Domain.HGVS.ReferenceSequence hgvs.Transcript }
                | _ -> Error $"Invalid HGVS: {hgvs}"

            /// If the HGVS is present, validate it.
            let validateOptional json =
                let allPartsMissing =
                    [ json.``HGVS.p``
                      json.``HGVS.pFull``
                      json.``HGVS.c``
                      json.MutationEffect
                      json.Transcript
                    ] |> List.forall String.isBlank

                if allPartsMissing then
                    Ok None
                else
                    validate json
                    |> Result.map Some

        module Variant =
            module NucleotideAlteration =
                open StringValidations.Typed

                /// Validate that a nucleotide alteration is not blank
                let validate = validateNotBlank Domain.Variant.NucleotideAlteration "Nucleotide alteration cannot be blank"

                /// Validate a nucleotide alteration, if it exists.
                let validateOptional = Optional.validateWith validate

            module AllelicFraction =
                /// Validate that the input for an allelic fraction is a parseable float greater than or equal to 0.0
                let (|ValidFraction|_|) =
                    Float.tryParse
                    >> Option.bind (fun num ->
                        if num >= 0.0 then Some num
                        else None)
                    >> Option.map Domain.Variant.AllelicFraction

                /// Validate that an allelic fraction is a parseable float greater than or equal to 0.
                ///
                ///    validate "10.0" = Ok (Domain.Variant.AllelicFraction 10.0)
                ///    validate "-10.0" = Error "Invalid allelic fraction: -10.0"
                let validate input =
                    match input with
                    | ValidFraction allelicFraction -> Ok allelicFraction
                    | _ -> Error $"Invalid allelic fraction: {input}"

                /// Validate an allelic fraction, if it exists.
                let validateOptional = Optional.validateWith validate

            module Description =
                open StringValidations.Typed

                /// Validate that a variant description is not blank
                let validate = validateNotBlank Domain.Variant.Description $"Variant description can't be blank"


        /// Logic for the `somaticPotentiallyActionableMutations` subsection of the report's `results` section
        module SomaticPotentiallyActionable =
            /// The json object for the `variants` section in each `somatic potentially actionable mutation`
            type Variant =
                { HgvsJson: HGVS
                  NucleotideAlteration: string option
                  AllelicFraction: string option
                  VariantDescription: string }

                static member Decoder : Decoder<Variant> =
                    Decode.object (fun get ->
                        { HgvsJson = get.Required.Raw HGVS.Decoder
                          NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decoder.Optional.string
                          AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decoder.Optional.string
                          VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                        } )

            module Variant =
                open FsToolkit.ErrorHandling

                /// Validate a somatic, potentially actionable variant's hgvs, nucleotide alteration, allelic fraction, and variant description
                let validate (json: Variant) : Validation<Domain.SomaticPotentiallyActionable.Variant,string> =
                    validation {
                        let! hgvs = json.HgvsJson |> HGVS.validate
                        and! nucleotideAlteration = json.NucleotideAlteration |> Variant.NucleotideAlteration.validateOptional
                        and! allelicFraction      = json.AllelicFraction |> Variant.AllelicFraction.validateOptional
                        and! variantDescription   = json.VariantDescription |> Variant.Description.validate

                        return ({
                            HGVS = hgvs
                            NucleotideAlteration = nucleotideAlteration
                            AllelicFraction = allelicFraction
                            Description = variantDescription
                        } : Domain.SomaticPotentiallyActionable.Variant )
                    }

            module Variants =
                open FsToolkit.ErrorHandling

                /// Validate a list of somatic, potentially actionable variants and return either a list of successfully validated variants or a list of errors
                let validate =
                    List.map Variant.validate
                    >> Result.combine
                    >> Result.mapError List.flatten

            type Mutation =
                { GeneJson: Gene
                  VariantJsons: Variant list }

                static member Decoder =
                    Decode.object (fun get ->
                        { GeneJson = get.Required.Raw Gene.Decoder
                          VariantJsons = "variants" |> flip get.Required.Field (Decode.list Variant.Decoder)
                        })

            module Mutation =
                open FsToolkit.ErrorHandling

                /// Validate that somatic, potentially actionable mutation has a valid gene and variants.
                let validate (mutation: Mutation) : Validation<Domain.SomaticPotentiallyActionable.Mutation, string> =
                    validation {
                        let! gene = mutation.GeneJson |> Gene.validate
                        and! variants = mutation.VariantJsons |> Variants.validate

                        return ({
                            Gene = gene
                            Variants = variants
                        } : Domain.SomaticPotentiallyActionable.Mutation) }

            module Mutations =
                open FsToolkit.ErrorHandling

                /// Validate a list of somatic, potentially actionable mutations
                let validate =
                    List.map Mutation.validate
                    >> Result.combine
                    >> Result.mapError List.flatten

            type CopyNumberVariant =
                { Gene: Gene
                  VariantDescription: string
                  VariantType: string }

                static member Decoder : Decoder<CopyNumberVariant> =
                    Decode.object (fun get ->
                        { Gene = get.Required.Raw Gene.Decoder
                          VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                          VariantType = "variantType" |> flip get.Required.Field Decode.string
                        } )

            /// logic for `somaticPotentiallyActionableCopyNumberVariants` subsection of the `results` section
            module CopyNumberVariant =
                open Domain.SomaticPotentiallyActionable.CopyNumberVariant

                module Description =
                    /// Validate that a copy number variant description is either 'copy number gain' or 'copy number loss'
                    let validate input =
                        match input with
                        | "Copy number gain" -> Ok ``Copy number gain``
                        | "Copy number loss" -> Ok ``Copy number loss``
                        | _ -> Error $"Invalid copy number variant description: {input}"

                module Type =
                    /// Validate that a copy number variant type is either 'amplification' or 'deletion'
                    let validate input =
                        match input with
                        | "amplification" -> Ok Amplification
                        | "deletion" -> Ok Deletion
                        | _ -> Error $"Invalid copy number variant type: {input}"


                open FsToolkit.ErrorHandling

                /// Validate a copy number variant has a valid gene, description, and type
                let validate (cnv: CopyNumberVariant) =
                    validation {
                        let! gene = cnv.Gene |> Gene.validate
                        and! variantDescription = cnv.VariantDescription |> Description.validate
                        and! variantType = cnv.VariantType |> Type.validate

                        return ({
                            Gene = gene
                            Description = variantDescription
                            Type = variantType
                        } : Domain.SomaticPotentiallyActionable.CopyNumberVariant ) }


            module CopyNumberVariants =
                open FsToolkit.ErrorHandling

                /// Validate a collection of somatic potentially actionable copy number variants
                let validate =
                    List.map CopyNumberVariant.validate
                    >> Result.combine
                    >> Result.mapError List.flatten

        /// Represents a json object found in `results.somaticBiologicallyRelevantVariants`
        type SomaticBiologicallyRelevantVariant =
            { GeneJson: Gene
              FusionJson: Fusion
              HgvsJson: HGVS
              NucleotideAlteration: string option
              AllelicFraction: string option
              VariantType: string
              VariantDescription: string
              StructuralVariant: string option }

            static member Decoder : Decoder<SomaticBiologicallyRelevantVariant> =
                Decode.object (fun get ->
                    { GeneJson   = Gene.Decoder   |> get.Required.Raw
                      FusionJson = Fusion.Decoder |> get.Required.Raw
                      HgvsJson   = HGVS.Decoder   |> get.Required.Raw
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                      StructuralVariant    = "structuralVariant"    |> flip get.Required.Field Decoder.Optional.string
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decoder.Optional.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decoder.Optional.string
                    } )

        module SomaticBiologicallyRelevantVariant =
            module Gene =
                open type Domain.SomaticBiologicallyRelevant.Mutation

                /// Validate that a gene or a fusion gene is present and valid
                let validate geneJson fusionGeneJson =
                  match Gene.validate geneJson, Fusion.validate fusionGeneJson with
                  | (Ok gene, Ok fusionGene) -> Error $"Both gene and fusion gene json are valid: ({gene}, {fusionGene})"
                  | (Error geneError, Error fusionError) -> Error $"Both gene and fusion gene are invalid: ({geneError}, {fusionError})"
                  | (Ok gene, _) -> Ok <| RelevantGene gene
                  | (_, Ok fusion) -> Ok <| RelevantFusion fusion

            module Type =
                open type Domain.SomaticBiologicallyRelevant.Type

                /// Validate that a somatic biologically relevant variant type is either 'CNV', 'SNV', or 'fusion'
                let validate input =
                    match input with
                    | "CNV" -> Ok CNV
                    | "SNV" -> Ok SNV
                    | "fusion" -> Ok Fusion
                    | _ -> Error $"Invalid somatic biologically relevant variant type: {input}"

            open FsToolkit.ErrorHandling

            /// Validate that a somatic, biologically relevant variant has either a gene or a fusion of genes, hgvs, a variant type of cnv, snv, or fusion, and a valid nucleotide alteration and allelic fraction.
            let validate (variant: SomaticBiologicallyRelevantVariant) =
                validation {
                    let! fusionOrGene = (variant.GeneJson, variant.FusionJson) ||> Gene.validate
                    and! hgvs = variant.HgvsJson |> HGVS.validateOptional
                    and! variantType = variant.VariantType |> Type.validate
                    and! nucleotideAlteration = variant.NucleotideAlteration |> Variant.NucleotideAlteration.validateOptional
                    and! allelicFraction = variant.AllelicFraction |> Variant.AllelicFraction.validateOptional

                    return ({
                        Mutation = fusionOrGene
                        HGVS     = hgvs
                        Type     = variantType
                        AllelicFraction      = allelicFraction
                        NucleotideAlteration = nucleotideAlteration
                    } : Domain.SomaticBiologicallyRelevant.Variant ) }

        module SomaticBiologicallyRelevantVariants =
            /// Validate a list of somatic biologically relevant variant inputs
            let validate =
                List.map SomaticBiologicallyRelevantVariant.validate
                >> Result.combine
                >> Result.mapError List.flatten

        type SomaticVUS =
            { GeneJson: Gene
              HgvsJson: HGVS
              NucleotideAlteration: string
              AllelicFraction: string
              VariantType: string
              VariantDescription: string }

            static member Decoder : Decoder<SomaticVUS> =
                Decode.object (fun get ->
                    { GeneJson = Gene.Decoder |> get.Required.Raw
                      HgvsJson = HGVS.Decoder |> get.Required.Raw
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string }
                )

        module SomaticVUS =
            module Type =
                open type Domain.SomaticVUS.Type

                let validate input =
                    match input with
                    | "SNV" -> Ok SNV
                    | _ -> Error $"Invalid VUS variant type: {input}"

            open FsToolkit.ErrorHandling

            /// Validate that a variant of unknown significance has a valid gene, hgvs, allelic fraction, nucleotide alteration, and variant type and description.
            let validate (json: SomaticVUS) : Validation<Domain.SomaticVUS, string> =
                validation {
                    let! gene = json.GeneJson |> Gene.validate
                    and! hgvs = json.HgvsJson |> HGVS.validate
                    and! allelicFraction = json.AllelicFraction |> Variant.AllelicFraction.validate
                    and! variantType = json.VariantType |> Type.validate
                    and! variantDescription = json.VariantDescription |> Variant.Description.validate
                    and! nucleotideAlteration = json.NucleotideAlteration |> Variant.NucleotideAlteration.validate

                    return ({
                        Gene = gene
                        Hgvs = hgvs
                        AllelicFraction = allelicFraction
                        Type = variantType
                        NucleotideAlteration = nucleotideAlteration
                        Description = variantDescription
                    } : Domain.SomaticVUS) }

        module SomaticVUSes =
            /// Validate a collection of somatic variants of unknown significance
            let validate =
                List.map SomaticVUS.validate
                >> Result.combine
                >> Result.mapError List.flatten

        type InheritedVariantValue =
            { Gene: Gene
              Hgvs: HGVS
              Description: string
              ClinicalSignificance: string
              Disease: string
              AllelicFraction: string
              Chromosome: uint
              ReferenceNucleotide: string
              AlteredNucleotide: string
              Position: uint
            }

            static member Decoder : Decoder<InheritedVariantValue> =
                Decode.object (fun get ->
                  { Gene = get.Required.Raw Gene.Decoder
                    Hgvs = get.Required.Raw HGVS.Decoder
                    Description          = "variantDescription"   |> flip get.Required.Field Decode.string
                    ClinicalSignificance = "clinicalSignificance" |> flip get.Required.Field Decode.string
                    Disease              = "disease"              |> flip get.Required.Field Decode.string
                    AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                    Chromosome           = "chromosome"           |> flip get.Required.Field Decode.uint32
                    ReferenceNucleotide  = "ref" |> flip get.Required.Field Decode.string
                    AlteredNucleotide    = "alt" |> flip get.Required.Field Decode.string
                    Position             = "pos" |> flip get.Required.Field Decode.uint32
                  } )

        type InheritedVariants =
            { NoteJson: string option
              Values: InheritedVariantValue list }

            static member Decoder : Decoder<InheritedVariants> =
                Decode.object (fun get ->
                    { NoteJson = "note"   |> flip get.Required.Field Decoder.Optional.string
                      Values   = "values" |> flip get.Required.Field (Decode.list InheritedVariantValue.Decoder) }
                )
        /// generic logic inherited variant sections: `results.inheritedRelevantVariants`, `results.inheritedVariantsOfUnknownSignficance`
        module InheritedVariants =
            open StringValidations.Typed

            module Note =
                open type Domain.InheritedVariants.Note

                /// Validate that an inherited variant note is not blank.
                let validate = validateNotBlank Note "Inherited variant note cannot be blank"
                /// Validate that if an inherited variant note exists, it is not blank.
                let validateOptional = Optional.validateWith validate


            module Value =
                module Disease =
                    open type Domain.InheritedVariants.Disease

                    /// Validate that an inherited variant disease is not blank.
                    let validate = validateNotBlank Disease "Inherited variant disease cannot be blank"

                module VariantDescription =
                    open type Domain.Variant.Description

                    /// Validate that an inherited variant description is not blank.
                    let validate = validateNotBlank Description "Inherited variant description cannot be blank"

                module ReferenceNucleotide =
                    open type Domain.InheritedVariants.ReferencedNucleotide

                    /// Validate that an inherited variant referenced nucleotide is not blank.
                    let validate = validateNotBlank ReferencedNucleotide "Reference nucleotide cannot be blank"

                module AlteredNucleotide =
                    open type Domain.InheritedVariants.AlteredNucleotide

                    /// Validate that an inherited variant altered nucleotide is not blank.
                    let validate = validateNotBlank AlteredNucleotide "Altered nucleotide cannot be blank"


                open FsToolkit.ErrorHandling

                /// a type abbreviation for a function that takes in a clinical significance input and returns either a valid clinical significance or an error message
                type ClinicalSignificanceValidator<'clinicalSignificance> = (string -> Result<'clinicalSignificance, string>)

                /// Validate that an inherited variant value has a valid gene, hgvs, description, clinical significnace, disease, allelic fraction, referenced nucleotide, and altered nucleotide
                let validate (validateClinicalSignificance: ClinicalSignificanceValidator<'clinicalSignificance>) (value: InheritedVariantValue) : Validation<Domain.InheritedVariants.Value<'clinicalSignificance>, string> =
                    validation {
                        let! gene = value.Gene |> Gene.validate
                        and! hgvs = value.Hgvs |> HGVS.validate
                        and! variantDescription = value.Description |> VariantDescription.validate
                        and! clinicalSignificance = value.ClinicalSignificance |> validateClinicalSignificance
                        and! disease = value.Disease |> Disease.validate
                        and! allelicFraction = value.AllelicFraction |> Variant.AllelicFraction.validate
                        and! referenceNucleotide = value.ReferenceNucleotide |> ReferenceNucleotide.validate
                        and! alteredNucleotide = value.AlteredNucleotide |> AlteredNucleotide.validate

                        let chromosome = value.Chromosome |> Domain.InheritedVariants.Chromosome
                        let position = value.Position |> Domain.InheritedVariants.Position

                        return ({
                            Gene = gene
                            HGVS = hgvs
                            Description = variantDescription
                            ClinicalSignificance = clinicalSignificance
                            Disease = disease
                            AllelicFraction = allelicFraction
                            ReferencedNucleotide = referenceNucleotide
                            AlteredNucleotide = alteredNucleotide
                            Chromosome = chromosome
                            Position = position
                        } : Domain.InheritedVariants.Value<'clinicalSignificance> )
                    }

            module Values =
                /// Validate a collection of inherited variant values
                let validate clinicalSignificanceValidator =
                    List.map (Value.validate clinicalSignificanceValidator)
                    >> Result.combine
                    >> Result.mapError List.flatten



            open FsToolkit.ErrorHandling

            /// Validate an inherited relevant variant and any associated inherited relevant variant values
            let validate (clinicalSignificanceValidator: Value.ClinicalSignificanceValidator<'clinicalSignificance>) (variants: InheritedVariants) : Validation<Domain.InheritedVariants<'clinicalSignificance>,string> =
                validation {
                    let! note = variants.NoteJson |>Note.validateOptional
                    and! values = variants.Values |> Values.validate clinicalSignificanceValidator

                    return ({
                        Note = note
                        Values = values
                    } : Domain.InheritedVariants<'clinicalSignificance>) }

        /// logic `results.inheritedRelevantVariants` section
        module InheritedRelevantVariants =
            module ClinicalSignificance =
                open type Domain.InheritedRelevantVariants.ClinicalSignificance

                /// Validate that an inherited relevant variant's clinical signficance is either "Likely Pathogenic", "Pathogenic", "Risk Allele", or "VUS Favoring Pathogenic"
                let validate input =
                    match input with
                    | "Likely Pathogenic" -> Ok ``Likely Pathogenic``
                    | "Pathogenic" -> Ok Pathogenic
                    | "Risk Allele" -> Ok ``Risk Allele``
                    | "VUS Favoring Pathogenic" -> Ok ``VUS Favoring Pathogenic``
                    | _ -> Error $"Invalid inherited relevant variant clinical significance: {input}"

            open FsToolkit.ErrorHandling

            /// Validate an inherited relevant variant and any associated inherited relevant variant values
            let validate (variant: InheritedVariants) : Validation<Domain.InheritedRelevantVariants,string> =
                variant |> InheritedVariants.validate ClinicalSignificance.validate

        /// logic for `results.InheritedVariantsOfUnknownSignficance` section
        module InheritedVUS =
            module ClinicalSignificance =
                open type Domain.InheritedVUS.ClinicalSignificance

                /// Validate that a VUS clinical signficance is "Variant of Unknown Significance"
                let validate input =
                    match input with
                    | "Variant of Unknown Significance" -> Ok ``Variant of Unknown Significance``
                    | _ -> Error $"Invalid VUS clinical significance: {input}"

            open FsToolkit.ErrorHandling

            /// Validate inherited variants of unknown significance
            let validate (jsons: InheritedVariants) : Validation<Domain.InheritedVUS,string> =
                jsons |> InheritedVariants.validate ClinicalSignificance.validate


    /// The `results` section in the Tempus report
    module Results =
        module TumorMutationBurden =
            type Input =
                { ScoreInput: ScoreInput
                  PercentileInput: PercentileInput }
            and ScoreInput = ScoreInput of float
            and PercentileInput = PercentileInput of uint

            /// Validate that tmb score and percentile are both present or absent.
            ///
            ///    validateOptional None None = Ok None
            ///    validateOptional (Some (ScoreInput 1.0)) (Some (PercentileInput 34)) = Ok
            let validateOptional (scoreInput: ScoreInput option) (percentileInput: PercentileInput option) =
                match scoreInput, percentileInput with
                | Some (ScoreInput score), Some (PercentileInput percentile) -> Ok <| Some { Score = TumorMutationBurdenScore score; Percentile = TumorMutationBurdenPercentile percentile }
                | None, None -> Ok None
                | Some _, None -> Error $"TMB percentile is missing: {scoreInput}"
                | None, Some _ -> Error $"TMB score is missing: {scoreInput}"

        module MicrosatelliteInstabilityStatus =
            type Input = Input of string

            open StringValidations.Typed

            /// Validate that msi status is not blank
            let validate (Input input) =
                input |> validateNotBlank MicrosatelliteInstabilityStatus "MSI status can't be blank"

            /// Validate that if an msi status, if it exists, is not blank
            let validateOptional (input: Input option) =
                input |> Optional.validateWith validate

        type Json =
            { TumorMutationBurden: float option
              TumorMutationBurdenPercentile: uint option
              MsiStatus: string option
              ``Somatic Potentially Actionable Mutations``: ``Somatic Potentially Actionable Mutation``.Json list
              ``Somatic Potentially Actionable Copy Number Variants``: ``Somatic Potentially Actionable Copy Number Variant``.Json list
              ``Somatic Biologically Relevant Variants``: ``Somatic Biologically Relevant Variant``.Json list
              ``Somatic Variants of Unknown Significance``: ``Somatic Variant of Unknown Significance``.Json list
              Fusions: Fusion.Json list
              ``Inherited Relevant Variants``: InheritedVariants.Json
              ``Inherited Variants Of Unknown Significance``: InheritedVariants.Json }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    // microsatellite instability status will either be a string field or an object that might have a 'Status' field
                    let msiObject = ["microsatelliteInstability"; "status"] |> flip get.Optional.At Decoder.Optional.string // object with optional "status" field
                    let msiStringField = "msiStatus" |> flip get.Optional.Field Decoder.Optional.string // field could be null, blank string, or actual value
                    let msiStatus = msiStringField |> Option.orElse msiObject |> Option.flatten

                    { TumorMutationBurden           = "tumorMutationalBurden"         |> flip get.Required.Field Decoder.Optional.float // can either be a float, blank string (i.e. ""), or null
                      TumorMutationBurdenPercentile = "tumorMutationBurdenPercentile" |> flip get.Required.Field Decoder.Optional.unsignedInteger // can either be an integer, blank string, or null
                      MsiStatus                     = msiStatus
                      ``Somatic Potentially Actionable Mutations`` = "somaticPotentiallyActionableMutations" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Mutation``.Json.Decoder)
                      ``Somatic Potentially Actionable Copy Number Variants`` = "somaticPotentiallyActionableCopyNumberVariants" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Copy Number Variant``.Json.Decoder)
                      ``Somatic Biologically Relevant Variants``   = "somaticBiologicallyRelevantVariants"  |> flip get.Required.Field (Decode.list ``Somatic Biologically Relevant Variant``.Json.Decoder)
                      ``Somatic Variants of Unknown Significance`` = "somaticVariantsOfUnknownSignificance" |> flip get.Required.Field (Decode.list ``Somatic Variant of Unknown Significance``.Json.Decoder)
                      Fusions = "fusionVariants" |> flip get.Required.Field (Decode.list Fusion.Json.Decoder)
                      ``Inherited Relevant Variants`` = "inheritedRelevantVariants" |> flip get.Required.Field InheritedVariants.Json.Decoder
                      ``Inherited Variants Of Unknown Significance`` = "inheritedVariantsOfUnknownSignificance" |> flip get.Required.Field InheritedVariants.Json.Decoder
                    } )

        open FsToolkit.ErrorHandling

        /// Validate the `results` section of the json report
        let validate (json: Json) =
            validation {
                let! tmb = (json.TumorMutationBurden           |> Option.map TumorMutationBurden.ScoreInput,
                            json.TumorMutationBurdenPercentile |> Option.map TumorMutationBurden.PercentileInput) ||> TumorMutationBurden.validateOptional
                and! msiStatus = json.MsiStatus |> Option.map MicrosatelliteInstabilityStatus.Input |> MicrosatelliteInstabilityStatus.validateOptional
                and! somaticPotentiallyActionableMutations = json.``Somatic Potentially Actionable Mutations`` |> ``Somatic Potentially Actionable Mutations``.validate
                and! somaticPotentiallyActionableCopyNumberVariants = json.``Somatic Potentially Actionable Copy Number Variants`` |> ``Somatic Potentially Actionable Copy Number Variants``.validate
                and! somaticPotentiallyRelevantVariants = json.``Somatic Biologically Relevant Variants`` |> ``Somatic Biologically Relevant Variants``.validate
                and! somaticVariantsOfUnknownSignificance = json.``Somatic Variants of Unknown Significance`` |> ``Somatic Variants of Unknown Significance``.validate
                and! fusions = json.Fusions |> Fusions.validate
                and! inheritedRelevantVariants = json.``Inherited Relevant Variants`` |> ``Inherited Relevant Variants``.validate
                and! inheritedVUS = json.``Inherited Variants Of Unknown Significance`` |> ``Inherited Variants Of Unknown Significance``.validate

                return { TumorMutationBurden = tmb
                         MicrosatelliteInstabilityStatus = msiStatus
                         ``Somatic Potentially Actionable Mutations`` = somaticPotentiallyActionableMutations
                         ``Somatic Potentially Actionable Copy Number Variants`` = somaticPotentiallyActionableCopyNumberVariants
                         ``Somatic Biologically Relevant Variants`` = somaticPotentiallyRelevantVariants
                         ``Somatic Variants of Unknown Significance`` = somaticVariantsOfUnknownSignificance
                         Fusions = fusions
                         ``Inherited Relevant Variants`` = inheritedRelevantVariants
                         ``Inherited Variants of Unknown Significance`` = inheritedVUS
                       } }

    type Json =
        { Order: Order.Json
          Lab: Lab.Json
          Report: Report.Json
          Patient: Patient.Json
          Samples: Sample.Json list
          Results: Results.Json }

          /// deserialize the json file as a whole: the lab, report, patient, order, and specimens object
          static member Decoder : Decoder<Json> =
              Decode.object (fun get ->
                  { Lab     = "lab"       |> flip get.Required.Field Lab.Json.Decoder
                    Report  = "report"    |> flip get.Required.Field Report.Json.Decoder
                    Patient = "patient"   |> flip get.Required.Field Patient.Json.Decoder
                    Order   = "order"     |> flip get.Required.Field Order.Json.Decoder
                    Samples = "specimens" |> flip get.Required.Field (Decode.list Sample.Json.Decoder)
                    Results = "results"   |> flip get.Required.Field Results.Json.Decoder
                  } )

            member this.TumorSample =
                this.Samples |> Seq.find (fun sample -> sample.SampleCategory = "tumor")

            member this.TryNormalSample =
                this.Samples |> Seq.tryFind (fun sample -> sample.SampleCategory = "normal")

    module OverallReport =
        let patientHasMrn (overallReport: OverallReport) =
            overallReport.Patient.MRN.IsSome

    module Json =
        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString Json.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })

        open FsToolkit.ErrorHandling

        /// Validate an overall report
        let validate (json: Json) =
            validation {
                let! lab     = json.Lab     |> Lab.validate
                and! report  = json.Report  |> Report.validate
                and! patient = json.Patient |> Patient.validate
                and! order   = json.Order   |> Order.Json.validate
                and! tumorSample  = json.TumorSample     |> TumorSample.validate
                and! normalSample = json.TryNormalSample |> NormalSample.validateOptional
                and! results = json.Results |> Results.validate

                return { Lab     = lab
                         Report  = report
                         Patient = patient
                         Order   = order
                         TumorSample  = tumorSample
                         NormalSample = normalSample
                         Results = results
                       } }

    module Database =
        open Database

        /// Build a row to be inserted into the `patients` database table if the Tempus report's patient has an MRN.
        let tryPatientRow (overallReport: OverallReport) =
            let row = context.Public.Patients.Create()
            let patient = overallReport.Patient

            patient.TryMrnValue
            |> Option.map (fun mrnValue ->

                row.Mrn          <- mrnValue
                row.LastName     <- patient.LastName.Value
                row.FirstName    <- patient.FirstName.Value
                row.DateOfBirth  <- patient.DateOfBirth.Value
                row.Sex          <- patient.Sex.Value

                row
            )

        /// Build a row to be inserted into the `vendors` database table.
        let toVendorRow (overallReport: OverallReport) =
            let row = context.Public.Vendors.Create()
            let lab = overallReport.Lab

            row.Name          <- lab.LabName.Value
            row.CliaNumber    <- lab.CliaNumber.Value
            row.StreetAddress <- lab.Address.StreetAddress.Value
            row.City          <- lab.Address.City.Value
            row.State         <- lab.Address.State.Value
            row.ZipCode       <- lab.Address.Zipcode.Value

            row

        /// Build a row to be inserted in the `reports` database table, if the associated patient has an MRN.
        let tryReportRow (overallReport: OverallReport) =
            let patient = overallReport.Patient

            patient.TryMrnValue
            |> Option.map (fun mrnValue ->
                let row = context.Public.Reports.Create()
                let lab = overallReport.Lab
                let report = overallReport.Report
                let order = overallReport.Order
                let results = overallReport.Results

                row.PatientMrn <- mrnValue
                row.VendorCliaNumber <- lab.CliaNumber.Value
                row.ReportId <- report.ReportId.Value.ToString()
                row.OrderingPhysician <- Some order.Physician.Value
                row.Pathologist <- Some report.SigningPathologist.Value
                row.IssuedDate <- report.SignoutDate.Value

                row.TumorMutationalBurden <- results.TryTmbScoreValue
                row.TumorMutationalBurdenPercentile <- results.TryTmbPercentileValue |> Option.map int
                row.MsiStatus <- results.TryMsiStatusValue

                row.DiagnosisName <- patient.DiagnosisName.Value
                row.DiagnosisDate <- patient.TryDiagnosisDateValue

                row
            )

        /// Build a row for the tumor sample to be inserted into the `samples` database table.
        let toTumorSampleRow (overallReport: OverallReport) =
            let tumorSample = overallReport.TumorSample
            let row = context.Public.Samples.Create()

            row.Category   <- "tumor"
            row.SampleId   <- tumorSample.SampleId.Value.ToString()
            row.BiopsySite <- tumorSample.SampleSite.Value
            row.SampleType <- tumorSample.SampleType.Value

            row

        /// Build a row for the normal sample, if it exists, to be inserted into the `samples` database table.
        let tryNormalSampleRow (overallReport: OverallReport) =
            overallReport.NormalSample
            |> Option.map (fun normalSample ->
                let row = context.Public.Samples.Create()

                row.Category   <- "normal"
                row.SampleId   <- normalSample.SampleId.Value.ToString()
                row.BiopsySite <- normalSample.SampleSite.Value
                row.SampleType <- normalSample.SampleType.Value

                row
            )

        /// Build a row for the tumor sample to be inserted into the `sample_reports` database table
        let toTumorSampleReportRow (overallReport: OverallReport) =
            let tumorSample = overallReport.TumorSample
            let report = overallReport.Report
            let row = context.Public.SampleReports.Create()

            row.SampleId <- tumorSample.SampleId.Value.ToString()
            row.ReportId <- report.ReportId.Value.ToString()
            row.BlockId  <- tumorSample.TryBlockIdValue

            row.CollectionDate <- tumorSample.SampleDates.CollectionDate.Value
            row.ReceiptDate    <- tumorSample.SampleDates.ReceivedDate.Value

            row.TumorPercentage <- tumorSample.TryTumorPercentageValue |> Option.map int

            row

        /// Build a row for the normal sample, if it exists, to be insterested into the `sample_reports` database table
        let tryNormalSampleReportRow (overallReport: OverallReport) =
            overallReport.NormalSample
            |> Option.map (fun normalSample ->

                let report = overallReport.Report
                let row = context.Public.SampleReports.Create()

                row.SampleId <- normalSample.SampleId.Value.ToString()
                row.ReportId <- report.ReportId.Value.ToString()
                row.BlockId  <- normalSample.TryBlockIdValue

                row.CollectionDate <- normalSample.SampleDates.CollectionDate.Value
                row.ReceiptDate    <- normalSample.SampleDates.ReceivedDate.Value

                row.TumorPercentage <- normalSample.TryTumorPercentageValue |> Option.map int

                row
            )

        let toGeneRows (overallReport: OverallReport) =
            let results = overallReport.Results
            let somaticPotentiallyActionableGenes = results.``Somatic Potentially Actionable Mutations`` |> List.map (fun mutation -> mutation.Gene)
            let somaticPotentiallyActionableCopyNumberGenes = results.``Somatic Potentially Actionable Copy Number Variants`` |> List.map (fun variant -> variant.Gene)
            let somaticBiologicallyRelevantGenes = results.``Somatic Biologically Relevant Variants`` |> List.collect (fun variant -> variant.Genes)
            let somaticVusGenes = results.``Somatic Variants of Unknown Significance`` |> List.map (fun vus -> vus.Gene)
            let fusionGenes = results.Fusions |> List.collect (fun fusion -> fusion.Genes)
            let inheritedRelevantGenes = results.``Inherited Relevant Variants``.Values |> List.map (fun irv -> irv.Gene)
            let inheritedVusGenes = results.``Inherited Variants of Unknown Significance``.Values |> List.map (fun ivus -> ivus.Gene)

            somaticPotentiallyActionableGenes
            @ somaticPotentiallyActionableCopyNumberGenes
            @ somaticBiologicallyRelevantGenes
            @ somaticVusGenes
            @ fusionGenes
            @ inheritedRelevantGenes
            @ inheritedVusGenes
            |> List.map (fun gene ->
                let row = context.Public.Genes.Create()

                row.Name <- gene.GeneName.Value
                row.HgncId <- Some gene.HgncId.Value

                row
            )

        /// Build variant rows to be inserted into the `variants` database table. This function assumes that an existing sample report exists in the dtabase.
        let toVariantRows (overallReport: OverallReport) =
            let results = overallReport.Results
            let sampleId = overallReport.TumorSample.SampleId.Value.ToString()
            let reportId = overallReport.Report.ReportId.Value.ToString()

            let sampleReportId =
                query {
                    for sampleReport in context.Public.SampleReports do
                    where (sampleReport.ReportId = reportId && sampleReport.SampleId = sampleId)
                    select sampleReport.Id
                    exactlyOne
                }

            results.``Somatic Potentially Actionable Mutations`` |> Seq.collect (fun mutation ->
                let row = context.Public.Variants.Create()

                row.GeneName <- mutation.Gene.GeneName.Value
                row.SampleReportId <- sampleReportId

                mutation.Variants
                |> Seq.map (fun variant ->
                    row.NucleotideAlteration <- variant.TryNucleotideAlterationValue
                    row.AllelicFraction <- variant.TryAllelicFractionValue
                    row.HgvsProtein <- variant.HGVS.ProteinChange

                    row
                )
            )