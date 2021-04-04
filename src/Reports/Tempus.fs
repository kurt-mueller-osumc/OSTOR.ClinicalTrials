namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    module Domain =
        open Core.Domain

        /// the `lab` section of the Tempus report
        type Lab =
            { LabName: Lab.Name
              CliaNumber: Lab.CliaNumber
              Address: Address }


        module Report =
            type Identifier =
                internal | Identifier of System.Guid
                member this.Value = this |> fun (Identifier id) -> id

            type SigningPathologist =
                internal | SigningPathologist of string
                member this.Value = this |> fun (SigningPathologist signingPathologist) -> signingPathologist

            type SignoutDate =
                internal | SignoutDate of System.DateTime
                member this.Value = this |> fun (SignoutDate signoutDate) -> signoutDate

        /// the `report` section of the Tempus report
        type Report =
            { ReportId: Report.Identifier
              SigningPathologist: Report.SigningPathologist
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


        module Diagnosis =
            type Name = internal | Name of string
            type Date = internal | Date of System.DateTime

        type Diagnosis =
            { Name: Diagnosis.Name
              Date: Diagnosis.Date option}


        module Patient =
            type TempusId =
                internal | TempusId of System.Guid
                member this.Value = this |> fun (TempusId tempusId) -> tempusId

            type Sex =
                internal | Male | Female
                member this.Value =
                    match this with
                    | Male -> "male"
                    | Female -> "female"


        /// the `patients` section in the Tempus report
        type Patient =
            { MRN: Patient.MRN option
              FirstName: Person.FirstName
              LastName: Person.LastName
              DateOfBirth: Patient.DateOfBirth
              Sex: Patient.Sex
              Diagnosis: Diagnosis }


        module Order =
            type Identifier  = internal Identifier of string
            type AccessionId = internal AccessionId of string
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
            { Institution: Order.Institution
              Physician: Order.Physician
              OrderId: Order.Identifier
              AccessionId: Order.AccessionId
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

        (*
            Fusions
        *)

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

        (*
            Tumor Mutation Burden
        *)

        module Results =
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


            module ``Somatic Potentially Actionable`` =
                type Variant =
                    { HGVS: HGVS.Variant
                      NucleotideAlteration: Variant.NucleotideAlteration option
                      AllelicFraction: Variant.AllelicFraction option
                      Description: Variant.Description }

                /// represents a mutation found in `results.somaticPotentiallActionableMutations` section of the report
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


            module ``Somatic Biologically Relevant`` =
                type Mutation =
                    internal
                    | Gene of Gene | Fusion of Fusion

                    member this.Genes =
                        match this with
                        | Gene gene -> [gene]
                        | Fusion fusion -> fusion.Genes

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

            module ``Somatic Variant of Unknown Significance`` =
                type Type = internal | SNV

            /// an entry found in `results.SomaticVariantsOfUnknownSignificance`
            type ``Somatic Variant of Unknown Significance`` =
                { Gene: Gene
                  Hgvs: HGVS.Variant
                  Type: ``Somatic Variant of Unknown Significance``.Type
                  Description: Variant.Description
                  NucleotideAlteration: Variant.NucleotideAlteration
                  AllelicFraction: Variant.AllelicFraction
                }

            module InheritedVariants =
                type Note = internal Note of string
                type Disease = internal Disease of Disease
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
                      AlteredNucleotide: AlteredNucleotide }

            type InheritedVariants<'ClinicalSignificance> =
                { Note: InheritedVariants.Note option
                  Values: InheritedVariants.Value<'ClinicalSignificance> list }

            module ``Inherited Relevant Variants`` =
                type ClinicalSignificance =
                    internal
                    | ``Likely Pathogenic``
                    | Pathogenic
                    | ``Risk Allele``
                    | ``VUS Favoring Pathogenic``

            module ``Inherited Variants of Unknown Significance`` =
                type ClinicalSignificance =
                    internal | ``Variant of Unknown Significance``

            type ``Inherited Relevant Variants`` = InheritedVariants<``Inherited Relevant Variants``.ClinicalSignificance>
            type ``Inherited Variants of Unknown Significance`` = InheritedVariants<``Inherited Variants of Unknown Significance``.ClinicalSignificance>

        type Results =
            { TumorMutationBurden: Results.TumorMutationBurden option
              MicrosatelliteInstabilityStatus: Results.MicrosatelliteInstabilityStatus option
              ``Somatic Potentially Actionable Mutations``: Results.``Somatic Potentially Actionable``.Mutation list
              ``Somatic Potentially Actionable Copy Number Variants``: Results.``Somatic Potentially Actionable``.CopyNumberVariant list
              ``Somatic Biologically Relevant Variants``: Results.``Somatic Biologically Relevant``.Variant list
              ``Somatic Variants of Unknown Significance``: Results.``Somatic Variant of Unknown Significance`` list
              Fusions: Fusion list
              ``Inherited Relevant Variants``: Results.``Inherited Relevant Variants``
              ``Inherited Variants of Unknown Significance``: Results.``Inherited Variants of Unknown Significance``
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

            /// Validate that a json object representing a gene has a gene name, hgnc id, and entrez id
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
            type Type = Type of string
            type VariantDescription = VariantDescription of string

            module Type =
                /// Validate that a fusion type is `gene`, for now.
                let validate (Type fusionType) =
                    match fusionType with
                    | "gene" -> Ok Domain.Fusion.GeneFusion
                    | _ -> Error $"Invalid fusion type: {fusionType}"

                /// Validate an optional fusion type if it exists.
                let validateOptional =
                    Optional.validateWith validate

            module VariantDescription =
                /// Validate that a fusion variant description is either "Chromosomal rearrangement" or "Deletion (exons 2-7)"
                let validate (VariantDescription description) =
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
                    and! fusionType = fusion.FusionType |> Option.map Type |> Type.validateOptional
                    and! variantDescription = fusion.VariantDescription |> VariantDescription |> VariantDescription.validate

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

    /// Logic for the `order` section of the Tempus report.
    module Order =
        /// An `order` json object
        type Json =
            { Institution: string
              Physician: string
              OrderId: string
              AccessionId: string
              OrderTest: TestJson }

            /// Deserializer for the 'order' json object.
            ///
            ///  The following object attributes can be camel-cased or snake-cased:
            /// - `"tempusOrderId"` / `"tempusOrder_id"`
            static member Decoder: Decoder<Json> =
                Decode.object (fun get ->
                    let orderIdDecoder = ["tempusOrderId"; "tempusOrder_id"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf

                    { Institution = "institution"  |> flip get.Required.Field Decode.string
                      Physician   = "physician"    |> flip get.Required.Field Decode.string
                      OrderId     = orderIdDecoder |> get.Required.Raw
                      AccessionId = "accessionId"  |> flip get.Required.Field Decode.string
                      OrderTest   = "test"         |> flip get.Required.Field TestJson.Decoder }
                )

        and TestJson =
            { Code: string
              Name: string
              Description: string }

            /// Deserializer for the actual test code, name, and description ran on the report's patient
            static member Decoder : Decoder<TestJson> =
                Decode.object (fun get ->
                    { Code        = "code" |> flip get.Required.Field Decode.string
                      Name        = "name" |> flip get.Required.Field Decode.string
                      Description = "description" |> flip get.Required.Field Decode.string }
                )

        module Test =
            open Utilities.StringValidations

            /// Validate that a test's code, name, and description are not blank
            let validate (json: TestJson) =
                match (json.Code, json.Name, json.Description) with
                | NotBlank, NotBlank, NotBlank -> Ok <| { TestCode = TestCode json.Code; TestName = TestName json.Name; TestDescription = TestDescription json.Description }
                | _ -> Error $"Either test code, name, or description is blank: {json}"

        module OrderId =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that a tempus order id is in the following format where the first character is a 0, 1 or 2, followed by one digit, follwed by four letters: `"(0|1|2)dxxxx"`
            ///
            ///    validate (Input "20hnyc") = Ok (OrderId "20hnyc")
            ///    validate (Input "30aaaa") = Error "Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: 30aaaa"
            let validate (Input input) =
                if Regex("^(0|1|2){1}\d{1}[a-zA-z]{4}$").Match(input).Success then
                    Ok <| OrderId input
                else
                    Error $"Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: {input}"

        module AccessionId =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that Tempus order's accession id is the in the following format where d is a digit and x is any alphanumeric character: `"TL-(0|1|2)d-xxxxxx"`
            ///
            ///    validate (Input "TL-19-DF60D1") = Ok (AccessionId "TL-19-DF60D1")
            //     validate (Input "TL-33-AAAAAA") = Error "Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: TL-33-AAAAAA"
            let validate (Input input) =
                if Regex("^TL-(0|1|2){1}\d{1}-(\d|[A-Z]|[a-z]){6}$").Match(input).Success then
                    Ok <| AccessionId input
                else
                    Error $"Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: {input}"

        module Json =
            open Utilities.StringValidations
            open FsToolkit.ErrorHandling

            /// Validate the input from the "order" section of the json report
            let validate (json: Json) =
                validation {
                    let! institution = json.Institution |> validateNotBlank |> Result.map Institution
                    and! physician = json.Physician |> validateNotBlank |> Result.map Physician
                    and! orderId = json.OrderId |> OrderId.Input |> OrderId.validate
                    and! accessionId = json.AccessionId |> AccessionId.Input |> AccessionId.validate
                    and! orderTest = json.OrderTest |> Test.validate

                    return { Institution = institution
                             Physician = physician
                             OrderId = orderId
                             AccessionId = accessionId
                             Test = orderTest
                           } }

    module Lab =
        type Json =
            { Name: string
              StreetAddress: string
              City: string
              State: string
              Zip: string
              CliaNumber: string }

            static member CamelCaseDecoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Name          = "name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "streetAddress" |> flip get.Required.Field Decode.string
                      City          = "city"          |> flip get.Required.Field Decode.string
                      State         = "state"         |> flip get.Required.Field Decode.string
                      Zip           = "zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "cliaNo"        |> flip get.Required.Field Decode.string }
                )

            static member PascalCaseDecoder : Decoder<Json> =
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
            static member Decoder : Decoder<Json> =
                Decode.oneOf [ Json.CamelCaseDecoder; Json.PascalCaseDecoder ]

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate the lab section of the json report
        let validate (json: Json) : Validation<Lab,string> =
            validation {
                let! labName = json.Name |> validateNotBlank |> Result.map LabName
                and! streetAddress = json.StreetAddress |> validateNotBlank |> Result.map StreetAddress
                and! city = json.City |> validateNotBlank |> Result.map City
                and! state = json.State |> validateNotBlank |> Result.map State
                and! zip = json.Zip |> validateNotBlank |> Result.map Zipcode
                and! cliaNumber = json.CliaNumber |> Lab.CliaNumber.Input |> Lab.CliaNumber.validate

                return { LabName = labName
                         Address = {
                             StreetAddress = streetAddress
                             City = city
                             State = state
                             Zipcode = zip
                         }
                         CliaNumber = cliaNumber
                       } }

    module Report =
        open System

        type Json =
            { ReportId: Guid
              SigningPathologist: string
              SignoutDate: DateTime }

            /// Deserializer for the "report" section of the Tempus report
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let pathologistDecoder = ["signing_pathologist"; "signingPathologist"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf
                    let signoutDateDecoder = ["signout_date"; "signoutDate"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { ReportId           = "reportId"         |> flip get.Required.Field Decode.guid
                      SigningPathologist = pathologistDecoder |> get.Required.Raw
                      SignoutDate        = signoutDateDecoder |> get.Required.Raw
                    } )

        open Utilities.StringValidations
        open FsToolkit.ErrorHandling

        /// Validate that a signing pathologist's name is not blank
        let validate (json: Json) : Validation<Report,string> =
            validation {
                let! signingPathologist = json.SigningPathologist |> validateNotBlank |> Result.map SigningPathologist

                return ({ ReportId = ReportId json.ReportId
                          SigningPathologist = signingPathologist
                          SignoutDate = SignoutDate json.SignoutDate} : Report
                       ) }

    module Sample =
        open System

        module SampleType =
            type Input = Input of string

            /// Validate that a sample type is either blood, block, slides, or saliva.
            let validate (Input input) =
                match input with
                | "Blood" -> Ok Blood
                | "FFPE Block" -> Ok ``FFPE Block``
                | "FFPE Slides (Unstained)" -> Ok ``FFPE Slides (Unstained)``
                | "Saliva" -> Ok Saliva
                | _ -> Error $"Invalid sample type: {input}"

        module CollectionDate =
            type Input = Input of DateTime

        module ReceivedDate =
            type Input = Input of DateTime

        module SampleDates =
            /// Validate that sample's collection date happens before its received date
            let validate (CollectionDate.Input collectionDate) (ReceivedDate.Input receivedDate) =
                if collectionDate < receivedDate then
                    Ok { CollectionDate = CollectionDate collectionDate; ReceivedDate = ReceivedDate receivedDate }
                else
                    Error $"Collection date, {collectionDate}, doesn't happen before received date, {receivedDate}."

        /// each entry in the `specimens` section of the Tempus report
        type Json =
            { SampleId: Guid
              CollectionDate: DateTime
              ReceivedDate: DateTime
              SampleCategory: string
              SampleSite: string
              SampleType: string
              Institution: InstitutionJson }

            /// Deserializer for the sample seciton of the Tempus report
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { SampleId       = "tempusSampleId"  |> flip get.Required.Field Decode.guid
                      CollectionDate = "collectionDate"  |> flip get.Required.Field Decode.datetime
                      ReceivedDate   = "receiptDate"     |> flip get.Required.Field Decode.datetime
                      SampleCategory = "sampleCategory"  |> flip get.Required.Field Decode.string
                      SampleSite     = "sampleSite"      |> flip get.Required.Field Decode.string
                      SampleType     = "sampleType"      |> flip get.Required.Field Decode.string
                      Institution    = "institutionData" |> flip get.Required.Field InstitutionJson.Decoder })

        and InstitutionJson =
            { BlockId: string option
              TumorPercentage: uint option }

            static member Decoder : Decoder<InstitutionJson> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.uint32
                    } )

    module TumorSample =
        module Category =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "tumor" -> Ok "tumor"
                | _ -> Error $"Sample category is not tumor: {input}"

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate a tumor sample
        let validate (json: Sample.Json) : Validation<TumorSample, string> =
            validation {
                // validate that the sample's listed category is 'tumor'
                let! tumorCategory = json.SampleCategory |> Category.Input |> Category.validate
                and! sampleDates   = (json.CollectionDate |> Sample.CollectionDate.Input, json.ReceivedDate |> Sample.ReceivedDate.Input) ||> Sample.SampleDates.validate
                and! sampleSite    = json.SampleSite |> validateNotBlank |> Result.map SampleSite
                and! sampleType    = json.SampleType |> Sample.SampleType.Input |> Sample.SampleType.validate

                return { SampleId = SampleId json.SampleId
                         SampleSite = sampleSite
                         SampleDates = sampleDates
                         SampleType = sampleType
                         BlockId = json.Institution.BlockId |> Option.map BlockId
                         TumorPercentage = json.Institution.TumorPercentage |> Option.map TumorPercentage
                       } }

    module NormalSample =
        module Category =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "normal" -> Ok "normal"
                | _ -> Error $"Sample category is not normal: {input}"

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate a normal sample
        let validate (json: Sample.Json) : Validation<NormalSample, string> =
            validation {
                // validate that the sample's listed category is 'nromal'
                let! normalCategory = json.SampleCategory |> Category.Input |> Category.validate
                and! sampleDates    = (json.CollectionDate |> Sample.CollectionDate.Input, json.ReceivedDate |> Sample.ReceivedDate.Input) ||> Sample.SampleDates.validate
                and! sampleSite     = json.SampleSite |> validateNotBlank |> Result.map SampleSite
                and! sampleType     = json.SampleType |> Sample.SampleType.Input |> Sample.SampleType.validate

                return { SampleId = SampleId json.SampleId
                         SampleSite = sampleSite
                         SampleDates = sampleDates
                         SampleType = sampleType
                         BlockId = json.Institution.BlockId |> Option.map BlockId
                         TumorPercentage = json.Institution.TumorPercentage |> Option.map TumorPercentage
                       } }

        /// Validate normal sample, if it's present
        let validateOptional  =
            Optional.validateWith validate

    module Patient =
        open System

        module Sex =
            type Input = Input of string

            /// Validate that sex is either `"(M|m)ale"` or `"(F|f)emale"`
            let validate (Input input) =
                match input with
                | "Male" | "male" -> Ok Male
                | "Female" | "female" -> Ok Female
                | _ -> Error $"Invalid sex: {input}"

        type Json =
            { FirstName: string
              LastName: string
              TempusId: Guid
              MrnJson: string option
              SexJson: string
              DateOfBirth: DateTime
              Diagnosis: string
              DiagnosisDate: DateTime option }

            /// Deserializer for the 'patient' json object.
            ///
            /// The following object attributes can be camel-cased or snake-cased:
            /// - `emrId` / `emr_id`
            /// - `dateOfBirth` / `DoB`
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let diagnosisDate = "diagnosisDate" |> flip get.Required.Field Decoder.Optional.dateTime
                    let mrnDecoder = ["emrId"; "emr_id"] |> List.map (flip Decode.field Decoder.Optional.string) |> Decode.oneOf
                    let dobDecoder = ["dateOfBirth"; "DoB"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      MrnJson       = get.Required.Raw mrnDecoder
                      SexJson       = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = get.Required.Raw dobDecoder
                      Diagnosis     = "diagnosis"   |> flip get.Required.Field Decode.string
                      DiagnosisDate = diagnosisDate }
                )

        open FsToolkit.ErrorHandling

        /// Validate a patient input: their name, mrn, and sex
        let validate (json: Json) =
            validation {
                let! firstName = json.FirstName |> FirstName.Input |> FirstName.validate
                and! lastName  = json.LastName  |> LastName.Input |> LastName.validate
                and! mrn       = json.MrnJson |> Option.map MRN.Input |> MRN.validateOptional
                and! sex       = json.SexJson |> Sex.Input |> Sex.validate

                return { MRN = mrn
                         FirstName = firstName
                         LastName = lastName
                         TempusId = json.TempusId
                         Sex = sex
                         DateOfBirth = json.DateOfBirth |> DateOfBirth
                         DiagnosisName = json.Diagnosis |> Diagnosis.Name
                         DiagnosisDate = json.DiagnosisDate |> Option.map DiagnosisDate
                       }}

    /// General logic for variants
    module Variant =
        module NucleotideAlteration =
            open Utilities.StringValidations

            type Input = Input of string

            /// Validate that a nucleotide alteration is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map NucleotideAlteration
                |> Result.mapError (fun _ -> $"Nucleotide alteration cannot be blank")

            /// Validate a nucleotide alteration, if it exists.
            let validateOptional  =
                Optional.validateWith validate

        module AllelicFraction =
            type Input = Input of string

            let (|ValidFraction|_|) (Input input) =
                input
                |> Float.tryParse
                |> Option.bind (fun num ->
                    if num >= 0.0 then Some num
                    else None)
                |> Option.map AllelicFraction

            /// Validate that an allelic fraction is either not present or is a valid, parseable float.
            let validate input =
                match input with
                | ValidFraction allelicFraction -> Ok allelicFraction
                | _ -> Error $"Invalid allelic fraction: {input}"

            /// Validate an allelic fraction, if it exists.
            let validateOptional =
                Optional.validateWith validate

        module Description =
            open StringValidations
            type Input = Input of string

            /// Validate that a variant description is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map VariantDescription
                |> Result.mapError (fun _ -> $"Variant description can't be blank: {input}")

    /// Logic for the `somaticPotentiallyActionableMutations` subsection of the report's `results` section
    module ``Somatic Potentially Actionable Mutation`` =
        module Variant =
            /// The json object for the `variants` section in each `somatic potentially actionable mutation`
            type Json =
                { HgvsJson: HGVS.Json
                  NucleotideAlteration: string option
                  AllelicFraction: string option
                  VariantDescription: string }

                static member Decoder : Decoder<Json> =
                    Decode.object (fun get ->
                        { HgvsJson = get.Required.Raw HGVS.Json.Decoder
                          NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decoder.Optional.string
                          AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decoder.Optional.string
                          VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                        } )

            open FsToolkit.ErrorHandling

            /// Validate a somatic, potentially actionable variant's hgvs, nucleotide alteration, allelic fraction, and variant description
            let validate (json: Json) : Validation<``Somatic Potentially Actionable Variant``,string> =
                validation {
                    let! hgvs = json.HgvsJson |> HGVS.validate
                    and! nucleotideAlteration = json.NucleotideAlteration |> Option.map Variant.NucleotideAlteration.Input |> Variant.NucleotideAlteration.validateOptional
                    and! allelicFraction      = json.AllelicFraction |> Option.map Variant.AllelicFraction.Input |> Variant.AllelicFraction.validateOptional
                    and! variantDescription   = json.VariantDescription |> Variant.Description.Input |> Variant.Description.validate

                    return { HGVS = hgvs
                             NucleotideAlteration = nucleotideAlteration
                             AllelicFraction = allelicFraction
                             VariantDescription = variantDescription
                           } }

        module Variants =
            open FsToolkit.ErrorHandling

            /// Validate a list of somatic, potentially actionable variants and return either a list of successfully validated variants or a list of errors
            let validate (json: Variant.Json list) =
                json
                |> Seq.map Variant.validate
                |> Seq.toList
                |> Result.combine
                |> Result.mapError List.flatten

        type Json =
            { GeneJson: Gene.Json
              VariantJsons: Variant.Json list }

            static member Decoder =
                Decode.object (fun get ->
                    { GeneJson = get.Required.Raw Gene.Json.Decoder
                      VariantJsons = "variants" |> flip get.Required.Field (Decode.list Variant.Json.Decoder)
                    })

        open FsToolkit.ErrorHandling

        /// Validate that somatic, potentially actionable mutation has a valid gene and variants.
        let validate (json: Json) : Validation<``Somatic Potentially Actionable Mutation``, string> =
            validation {
                let! gene = json.GeneJson |> Gene.Json.validate
                and! variants = json.VariantJsons |> Variants.validate

                return { Gene = gene
                         Variants = variants } }

    module ``Somatic Potentially Actionable Mutations`` =
        open FsToolkit.ErrorHandling

        /// Validate a list of somatic, potentially actionable mutaitons
        let validate (jsons: ``Somatic Potentially Actionable Mutation``.Json list) =
            jsons
            |> Seq.map ``Somatic Potentially Actionable Mutation``.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten

    /// logic for `somaticPotentiallyActionableCopyNumberVariants` subsection of the `results` section
    module ``Somatic Potentially Actionable Copy Number Variant`` =
        module Description =
            type Input = Input of string

            /// Validate that a copy number variant description is either 'copy number gain' or 'copy number loss'
            let validate (Input input) =
                match input with
                | "Copy number gain" -> Ok ``Copy number gain``
                | "Copy number loss" -> Ok ``Copy number loss``
                | _ -> Error $"Invalid copy number variant description: {input}"

        module Type =
            type Input = Input of string

            /// Validate that a copy number variant type is either 'amplification' or 'deletion'
            let validate (Input input) =
                match input with
                | "amplification" -> Ok Amplification
                | "deletion" -> Ok Deletion
                | _ -> Error $"Invalid copy number variant type: {input}"

        type Json =
            { Gene: Gene.Json
              VariantDescription: string
              VariantType: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene = get.Required.Raw Gene.Json.Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      VariantType = "variantType" |> flip get.Required.Field Decode.string
                    } )

        open FsToolkit.ErrorHandling

        /// Validate a copy number variant input
        let validate (json: Json) =
            validation {
                let! gene = json.Gene |> Gene.Json.validate
                and! variantDescription = json.VariantDescription |> Description.Input |> Description.validate
                and! variantType = json.VariantType |> Type.Input |> Type.validate

                return { Gene = gene
                         Description = variantDescription
                         Type = variantType
                       } }

    module ``Somatic Potentially Actionable Copy Number Variants`` =
        open FsToolkit.ErrorHandling

        /// Validate a collection of somatic potentially actionable copy number variants
        let validate (jsons: ``Somatic Potentially Actionable Copy Number Variant``.Json list) =
            jsons
            |> Seq.map ``Somatic Potentially Actionable Copy Number Variant``.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten


    module ``Somatic Biologically Relevant Variant`` =
        module Gene =
            /// Validate that a gene or a fusion gene is present and valid
            let validate geneJson fusionGeneJson =
              match Gene.Json.validate geneJson, Fusion.validate fusionGeneJson with
              | (Ok gene, Ok fusionGene) -> Error $"Both gene and fusion gene json are valid: ({gene}, {fusionGene})"
              | (Error geneError, Error fusionError) -> Error $"Both gene and fusion gene are invalid: ({geneError}, {fusionError})"
              | (Ok gene, _) -> Ok <| RelevantGene gene
              | (_, Ok fusion) -> Ok <| RelevantFusion fusion

        module Type =
            type Input = Input of string

            /// Validate that a somatic biologically relevant variant type is either 'CNV', 'SNV', or 'fusion'
            let validate (Input input) =
                match input with
                | "CNV" -> Ok CNV
                | "SNV" -> Ok RelevantSNV
                | "fusion" -> Ok Fusion
                | _ -> Error $"Invalid somatic biologically relevant variant type: {input}"

        /// Represents a json object found in `results.somaticBiologicallyRelevantVariants`
        type Json =
            { GeneJson: Gene.Json
              FusionJson: Fusion.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string option
              AllelicFraction: string option
              VariantType: string
              VariantDescription: string
              StructuralVariant: string option }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson   = Gene.Json.Decoder   |> get.Required.Raw
                      FusionJson = Fusion.Json.Decoder |> get.Required.Raw
                      HgvsJson   = HGVS.Json.Decoder   |> get.Required.Raw
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                      StructuralVariant    = "structuralVariant"    |> flip get.Required.Field Decoder.Optional.string
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decoder.Optional.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decoder.Optional.string
                    } )

        open FsToolkit.ErrorHandling

        module Json =
            /// Validate that a somatic, biologically relevant variant has either a gene or a fusion of genese, hgvs, a variant type of cnv, snv, or fusion, and valid nucleotide laterations and allelic fractions.
            let validate (json: Json) =
                validation {
                    let! fusionOrGene = (json.GeneJson, json.FusionJson) ||> Gene.validate
                    and! hgvs = json.HgvsJson |> HGVS.validateOptional
                    and! variantType = json.VariantType |> Type.Input |> Type.validate
                    and! nucleotideAlteration = json.NucleotideAlteration |> Option.map Variant.NucleotideAlteration.Input |> Variant.NucleotideAlteration.validateOptional
                    and! allelicFraction = json.AllelicFraction |> Option.map Variant.AllelicFraction.Input |> Variant.AllelicFraction.validateOptional

                    return { Gene = fusionOrGene
                             HGVS = hgvs
                             AllelicFraction = allelicFraction
                             NucleotideAlteration = nucleotideAlteration
                             Type = variantType
                    } }

    module ``Somatic Biologically Relevant Variants`` =
        /// Validate a list of somatic biologically relevant variant inputs
        let validate (jsons: ``Somatic Biologically Relevant Variant``.Json list) =
            jsons
            |> Seq.map ``Somatic Biologically Relevant Variant``.Json.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten

    module ``Somatic Variant of Unknown Significance`` =
        module Type =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "SNV" -> Ok SNV
                | _ -> Error $"Invalid VUS variant type: {input}"

        type Json =
            { GeneJson: Gene.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string
              VariantType: string
              VariantDescription: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson = Gene.Json.Decoder |> get.Required.Raw
                      HgvsJson = HGVS.Json.Decoder |> get.Required.Raw
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string }
                )

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate that a variant of unknown significance has a valid gene, hgvs, allelic fraction, nucleotide alteration, and variant type and description.
        let validate (json: Json) : Validation<``Somatic Variant of Unknown Significance``, string> =
            validation {
                let! gene = json.GeneJson |> Gene.Json.validate
                and! hgvs = json.HgvsJson |> HGVS.validate
                and! allelicFraction = json.AllelicFraction |> Variant.AllelicFraction.Input |> Variant.AllelicFraction.validate
                and! variantType = json.VariantType |> Type.Input |> Type.validate
                and! variantDescription = json.VariantDescription |> validateNotBlank |> Result.map VariantDescription
                and! nucleotideAlteration = json.NucleotideAlteration |> Variant.NucleotideAlteration.Input |> Variant.NucleotideAlteration.validate

                return { Gene = gene
                         Hgvs = hgvs
                         AllelicFraction = allelicFraction
                         Type = variantType
                         NucleotideAlteration = nucleotideAlteration
                         Description = variantDescription
                       } }

    module ``Somatic Variants of Unknown Significance`` =
        let validate (jsons: ``Somatic Variant of Unknown Significance``.Json list) =
            jsons
            |> Seq.map ``Somatic Variant of Unknown Significance``.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten

    /// generic logic inherited variant sections: `results.inheritedRelevantVariants`, `results.inheritedVariantsOfUnknownSignficance`
    module InheritedVariants =
        open StringValidations.Typed

        module Note =
            type Input = Input of string

            /// Validate that an inherited variant note is not blank.
            let validate (Input input) =
                input |> validateNotBlank Note "Inherited variant note cannot be blank"

            /// Validate that if an inherited variant note exists, it is not blank.
            let validateOptional =
                Optional.validateWith validate

        module Value =
            module ClinicalSignificance =
                /// placeholder variable for clinical significance inputs
                type Input = Input of string

            module Disease =
                type Input = Input of string

                /// Validate that an inherited variant disease is not blank.
                let validate (Input input) =
                    input |> validateNotBlank Disease "Inherited variant disease cannot be blank"

            module VariantDescription =
                type Input = Input of string

                /// Validate that an inherited variant descrption is not blank.
                let validate (Input input) =
                    input |> validateNotBlank VariantDescription "Inherited variant description cannot be blank"

            module ReferenceNucleotide =
                type Input = Input of string

                /// Validate that an inherited variant descrption is not blank.
                let validate (Input input) =
                    input |> validateNotBlank ReferencedNucleotide "Reference nucleotide cannot be blank"

            module AlteredNucleotide =
                type Input = Input of string

                /// Validate that an inherited variant descrption is not blank.
                let validate (Input input) =
                    input |> validateNotBlank AlteredNucleotide "Altered nucleotide cannot be blank"

            type Json =
                { Gene: Gene.Json
                  Hgvs: HGVS.Json
                  Description: string
                  ClinicalSignificance: string
                  Disease: string
                  AllelicFraction: string
                  Chromosome: uint
                  ReferenceNucleotide: string
                  AlteredNucleotide: string
                  Position: uint
                }

                static member Decoder : Decoder<Json> =
                    Decode.object (fun get ->
                      { Gene = get.Required.Raw Gene.Json.Decoder
                        Hgvs = get.Required.Raw HGVS.Json.Decoder
                        Description          = "variantDescription"   |> flip get.Required.Field Decode.string
                        ClinicalSignificance = "clinicalSignificance" |> flip get.Required.Field Decode.string
                        Disease              = "disease"              |> flip get.Required.Field Decode.string
                        AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                        Chromosome           = "chromosome"           |> flip get.Required.Field Decode.uint32
                        ReferenceNucleotide  = "ref" |> flip get.Required.Field Decode.string
                        AlteredNucleotide    = "alt" |> flip get.Required.Field Decode.string
                        Position             = "pos" |> flip get.Required.Field Decode.uint32
                      } )

            open FsToolkit.ErrorHandling

            /// a type abbreviation for a function that takes in a clinical significance input and returns either a valid clinical significance or an error message
            type ClinicalSignificanceValidator<'clinicalSignificance> = (ClinicalSignificance.Input -> Result<'clinicalSignificance, string>)

            /// Validate that an inherited variant value has a valid gene, hgvs, description, clinical significnace, disease, allelic fraction, referenced nucleotide, and altered nucleotide
            let validate (validateClinicalSignificance: ClinicalSignificanceValidator<'clinicalSignificance>) (json: Json) : Validation<InheritedVariantValue<'clinicalSignificance>, string> =
                validation {
                    let! gene = json.Gene |> Gene.Json.validate
                    and! hgvs = json.Hgvs |> HGVS.validate
                    and! variantDescription = json.Description |> VariantDescription.Input |> VariantDescription.validate
                    and! clinicalSignificance = json.ClinicalSignificance |> ClinicalSignificance.Input |> validateClinicalSignificance
                    and! disease = json.Disease |> Disease.Input |> Disease.validate
                    and! allelicFraction = json.AllelicFraction |> Variant.AllelicFraction.Input |> Variant.AllelicFraction.validate
                    and! referenceNucleotide = json.ReferenceNucleotide |> ReferenceNucleotide.Input |> ReferenceNucleotide.validate
                    and! alteredNucleotide = json.AlteredNucleotide |> AlteredNucleotide.Input |> AlteredNucleotide.validate

                    return ({ Gene = gene
                              Hgvs = hgvs
                              Description = variantDescription
                              ClinicalSignificance = clinicalSignificance
                              Disease = disease
                              AllelicFraction = allelicFraction
                              ReferencedNucleotide = referenceNucleotide
                              AlteredNucleotide = alteredNucleotide
                              Chromosome = Chromosome json.Chromosome
                              Position = Position json.Position
                            } )
                }

        module Values =
            let validate clinicalSignificanceValidator (jsons: Value.Json list) =
                jsons
                |> List.map (Value.validate clinicalSignificanceValidator)
                |> Result.combine
                |> Result.mapError List.flatten

        type Json =
            { NoteJson: string option
              Values: Value.Json list }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { NoteJson  = "note"   |> flip get.Required.Field Decoder.Optional.string
                      Values = "values" |> flip get.Required.Field (Decode.list Value.Json.Decoder) }
                )

        open FsToolkit.ErrorHandling

        /// Validate an inherited relevant variant and any associated inherited relevant variant values
        let validate (clinicalSignificanceValidator: Value.ClinicalSignificanceValidator<'clinicalSignificance>) (json: Json) : Validation<InheritedVariants<'clinicalSignificance>,string> =
            validation {
                let! note = json.NoteJson |> Option.map Note.Input |> Note.validateOptional
                and! values = json.Values |> Values.validate clinicalSignificanceValidator

                return { Note = note
                         Values = values } }

    /// logic `results.inheritedRelevantVariants` section
    module ``Inherited Relevant Variants`` =
        module ClinicalSignificance =
            open InheritedVariants.Value.ClinicalSignificance

            /// Validate that an inherited relevant variant's clinical signficance is either "Likely Pathogenic", "Pathogenic", "Risk Allele", or "VUS Favoring Pathogenic"
            let validate (Input input) =
                match input with
                | "Likely Pathogenic" -> Ok ``Likely Pathogenic``
                | "Pathogenic" -> Ok Pathogenic
                | "Risk Allele" -> Ok ``Risk Allele``
                | "VUS Favoring Pathogenic" -> Ok ``VUS Favoring Pathogenic``
                | _ -> Error $"Invalid inherited relevant variant clinical significance: {input}"

        open FsToolkit.ErrorHandling

        /// Validate an inherited relevant variant and any associated inherited relevant variant values
        let validate (jsons: InheritedVariants.Json) : Validation<``Inherited Relevant Variants``,string> =
            jsons |> InheritedVariants.validate ClinicalSignificance.validate

    /// logic for `results.InheritedVariantsOfUnknownSignficance` section
    module ``Inherited Variants Of Unknown Significance`` =
        module ClinicalSignificance =
            open InheritedVariants.Value.ClinicalSignificance

            /// Validate that a VUS clinical signficance is "Variant of Unknown Significance"
            let validate (Input input) =
                match input with
                | "Variant of Unknown Significance" -> Ok ``Variant of Unknown Significance``
                | _ -> Error $"Invalid VUS clinical significance: {input}"

        module Value =
            open FsToolkit.ErrorHandling

            let validate (json: InheritedVariants.Value.Json) : Validation<``Inherited Variant of Unknown Significance Value``, string> =
                InheritedVariants.Value.validate ClinicalSignificance.validate json

        open FsToolkit.ErrorHandling

        /// Validate inheerited variants of unknown significance
        let validate (jsons: InheritedVariants.Json) : Validation<``Inherited Variants of Unknown Significance``,string> =
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