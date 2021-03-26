namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Thoth.Json.Net
    open Utilities

    type Diagnosis =
        { DiagnosisName: DiagnosisName
          DiagnosisDate: DiagnosisDate option }
    and DiagnosisName = internal DiagnosisName of string
    and DiagnosisDate = internal DiagnosisDate of System.DateTime

    type TumorCategory = internal | TumorCategory
    type GermlineCategory = internal | GermlineCategory

    type Sample<'Category> =
        { SampleId: SampleId
          SampleSite: SampleSite
          SampleType: SampleType
          CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          TumorPercentage: TumorPercentage }
    and SampleId = internal SampleId of System.Guid
    and SampleSite = internal SampleSite of string
    and SampleType = internal SampleType of string
    and CollectionDate = internal CollectionDate of System.DateTime
    and ReceivedDate = internal ReceivedDate of System.DateTime
    and BlockId = internal BlockId of string
    and TumorPercentage = internal TumorPercentage of uint

    type Report =
        { Diagnosis: Diagnosis
          GermlineSample: Sample<GermlineCategory> option
          TumorSample: Sample<TumorCategory> }

    type Variant =
        internal
        | ``Somatic Biologically Relevant Variant`` of ``Somatic Biologically Relevant Variant``

    and ``Somatic Biologically Relevant Variant`` =
        { Gene: ``Somatic Biologically Relevant Gene``
          HGVS: HGVS option
          AllelicFraction: AllelicFraction option
          NucleotideAlteration: NucleotideAlteration option }

    and Gene =
        { GeneName: GeneName
          HgncId: HgncId
          EntrezId: EntrezId }
    and HgncId = internal HgncId of string
    and EntrezId = internal EntrezId of string

    and ``Somatic Biologically Relevant Gene`` =
        internal
        | Gene of Gene
        | FusionGene of FusionGene

    and FusionGene =
        { ``5' Gene``: Gene
          ``3' Gene``: Gene
          FusionType: FusionType }
    and FusionType = internal FusionType of string

    and HGVS =
        { ``Protein Sequence Change``: ``HGVS Protein Sequence Change`` option
          ``Coding DNA Sequence Change``: ``HGVS Coding DNA Sequence Change``
          ReferenceSequence: ReferenceSequence }

    and ``HGVS Protein Sequence Change`` =
        { AbbreviatedChange: ``Abbreviated Protein Sequence Change``
          FullChange: ``Full Protein Sequence Change`` }
    and ``Abbreviated Protein Sequence Change`` = internal ``Abbreviated Protein Sequence Change`` of string
    and ``Full Protein Sequence Change`` = internal ``Full Protein Sequence Change`` of string
    and ``HGVS Coding DNA Sequence Change`` = internal ``HGVS Coding DNA Sequence Change`` of string
    and ReferenceSequence = internal ReferenceSequence of string

    and VariantDescription = internal VariantDescription of string
    and VariantType = internal VariantType of string
    and NucleotideAlteration = internal NucleotideAlteration of string
    and AllelicFraction = internal AllelicFraction of float

    module Gene =
        /// Json object attributes that identifies genes
        type Json =
            { GeneId: string // 'gene' attribute
              HgncId: string
              EntrezId: string }

            /// Deserializes a gene's json object attributes
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene"     |> flip get.Required.Field Decode.string
                      HgncId   = "hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 5 json object attributes
            static member Gene5Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene5"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene5hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene5entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 3 json object attributes
            static member Gene3Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene3"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene3hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene3entrezId" |> flip get.Required.Field Decode.string })

        module Json =
            let (|NotBlank|BlankString|) string =
                if string = "" then BlankString
                else NotBlank

            let validate (json: Json) =
                match (json.GeneId, json.HgncId, json.EntrezId) with
                | (NotBlank, NotBlank, NotBlank) -> Ok { GeneName = GeneName json.GeneId; HgncId = HgncId json.HgncId; EntrezId = EntrezId json.EntrezId }
                | _ -> Error $"Gene missing name, hgnc id, or entrez id: {json}"

    module FusionGene =
        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        type Json =
            { Gene5Json: Gene.Json
              Gene3Json: Gene.Json
              FusionType: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene5Json  = get.Required.Raw Gene.Json.Gene5Decoder
                      Gene3Json  = get.Required.Raw Gene.Json.Gene3Decoder
                      FusionType = "fusionType" |> flip get.Required.Field Decode.string }
                )

        module FusionType =
            let validate =
                validateNotBlank
                >> Result.map FusionType
                >> Result.mapError (fun e -> $"Fusion type can't be blank: {e}")


        let validate (json: Json) =
            validation {
                let! gene5 = json.Gene5Json |> Gene.Json.validate
                and! gene3 = json.Gene3Json |> Gene.Json.validate
                and! fusionType = json.FusionType |> FusionType.validate

                return { ``5' Gene`` = gene5
                         ``3' Gene`` = gene3
                         FusionType = fusionType }
            }

    module HGVS =
        /// Represents HGVS reference sequences for proteins and coding DNA
        type Json =
            { ``HGVS.p``: string // abbreviated protein sequence change
              ``HGVS.pFull``: string // full protein sequence change
              ``HGVS.c``: string // coding DNA sequence change
              Transcript: string // the reference sequence (eg NM_012345.1)
              MutationEffect: string // will either equal 'HGVS.p' or 'HGVS.c'
            }

            /// Deserializer for hgvs json object attributes
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { ``HGVS.p``     = "HGVS.p"         |> flip get.Required.Field Decode.string
                      ``HGVS.pFull`` = "HGVS.pFull"     |> flip get.Required.Field Decode.string
                      ``HGVS.c``     = "HGVS.c"         |> flip get.Required.Field Decode.string
                      Transcript     = "transcript"     |> flip get.Required.Field Decode.string
                      MutationEffect = "mutationEffect" |> flip get.Required.Field Decode.string }
                )

        let mutationEffect hgvs =
            match hgvs.``Protein Sequence Change`` with
            | Some proteinSequenceChange ->
                let (``Abbreviated Protein Sequence Change`` sc) = proteinSequenceChange.AbbreviatedChange
                sc
            | _ ->
                let (``HGVS Coding DNA Sequence Change`` sc) = hgvs.``Coding DNA Sequence Change``
                sc


        open Utilities.StringValidations

        /// Validate a HGVS reference sequence:
        /// 1. all hgvs fields are blank OR
        /// 2. if hgvs.c is present and hgvs.p is blank, mutationEffect == hgvs.c OR
        /// 3. if hgvs.p is present
        ///    - hgvs.pFull (vice versa) is present
        ///    - hgvs.p == mutation effect
        let validate json =
            match (json.``HGVS.p``, json.``HGVS.pFull``, json.``HGVS.c``, json.MutationEffect, json.Transcript) with
            /// No HGVS protein sequence change present; only a coding DNA sequence is present
            | (BlankString, BlankString, NotBlank, NotBlank, NotBlank) when json.``HGVS.c`` = json.MutationEffect ->
                Ok <| { ``Protein Sequence Change`` = None
                        ``Coding DNA Sequence Change`` = ``HGVS Coding DNA Sequence Change`` json.``HGVS.c``
                        ReferenceSequence = ReferenceSequence json.Transcript }
            /// Both HGVS protein sequence change and coding DNA sequence change are present
            | (NotBlank, NotBlank, NotBlank, NotBlank, NotBlank) when json.``HGVS.p`` = json.MutationEffect ->
                Ok <| { ``Protein Sequence Change`` = Some { AbbreviatedChange = ``Abbreviated Protein Sequence Change`` json.``HGVS.p``
                                                             FullChange = ``Full Protein Sequence Change`` json.``HGVS.pFull`` }
                        ``Coding DNA Sequence Change`` = ``HGVS Coding DNA Sequence Change`` json.``HGVS.c``
                        ReferenceSequence = ReferenceSequence json.Transcript }
            | _ -> Error $"Invalid HGVS: {json}"

        /// If the HGVS is present, validate it.
        let validateOptional json =
            let hgvsMissing = [json.``HGVS.p``; json.``HGVS.pFull``; json.``HGVS.c``; json.MutationEffect; json.Transcript] |> List.forall String.isBlank

            if hgvsMissing then
                Ok None
            else
                validate json
                |> Result.map Some



    module Order =
        type Json =
            { Institution: string
              Physician: string
              OrderId: string
              AccessionId: string
              OrderTest: TestJson }

            /// Deserializer for the 'report' json object.
            ///
            ///  The following object attributes can be camel-cased or snake-cased:
            /// - tempusOrderId / tempusOrder_id
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

            static member Decoder : Decoder<TestJson> =
                Decode.object (fun get ->
                    { Code        = "code" |> flip get.Required.Field Decode.string
                      Name        = "name" |> flip get.Required.Field Decode.string
                      Description = "description" |> flip get.Required.Field Decode.string }
                )

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

    module Report =
        open System

        type Json =
            { ReportId: Guid
              SigningPathologist: string
              SignoutDate: DateTime }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let pathologistDecoder = ["signing_pathologist"; "signingPathologist"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf
                    let signoutDateDecoder = ["signout_date"; "signoutDate"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { ReportId           = "reportId"         |> flip get.Required.Field Decode.guid
                      SigningPathologist = pathologistDecoder |> get.Required.Raw
                      SignoutDate        = signoutDateDecoder |> get.Required.Raw }
                )

    module Sample =
        open System

        type Json =
            { SampleId: Guid
              CollectionDate: DateTime
              ReceivedDate: DateTime
              SampleCategory: string
              SampleSite: string
              SampleType: string
              Institution: InstitutionJson }

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
              TumorPercentage: int option }

            static member Decoder : Decoder<InstitutionJson> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.int }
                )

    module Patient =
        open System

        type Json =
            { FirstName: string
              LastName: string
              TempusId: Guid
              MRN: string option
              Sex: string
              DateOfBirth: DateTime
              Diagnosis: string
              DiagnosisDate: DateTime option }

            /// Deserializer for the 'patient' json object. The following object attributes can be camel-cased or snake-cased:
            /// - emrId / emr_id
            /// - dateOfBirth / DoB
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let diagnosisDate = "diagnosisDate" |> flip get.Required.Field Decoder.optionalDateTime
                    let mrnDecoder = ["emrId"; "emr_id"] |> List.map (flip Decode.field Decoder.optionalString) |> Decode.oneOf
                    let dobDecoder = ["dateOfBirth"; "DoB"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      MRN           = get.Required.Raw mrnDecoder
                      Sex           = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = get.Required.Raw dobDecoder
                      Diagnosis     = "diagnosis"   |> flip get.Required.Field Decode.string
                      DiagnosisDate = diagnosisDate }
                )

    module ``Somatic Potentially Actionable Mutation`` =
        module Variant =
            type Json =
                { HgvsJson: HGVS.Json
                  NucleotideAlteration: string
                  AllelicFraction: string
                  VariantDescription: string }

                static member Decoder : Decoder<Json> =
                    Decode.object (fun get ->
                        { HgvsJson = get.Required.Raw HGVS.Json.Decoder
                          NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                          AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                          VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                        }
                    )

            // let validate json =

        type Json =
            { GeneJson: Gene.Json
              VariantJsons: Variant.Json list }

            static member Decoder =
                Decode.object (fun get ->
                    { GeneJson = get.Required.Raw Gene.Json.Decoder
                      VariantJsons = "variants" |> flip get.Required.Field (Decode.list Variant.Json.Decoder) }
                )

    module ``Somatic Potentially Actionable Copy Number Variant`` =
        type Json =
            { Gene: Gene.Json
              VariantDescription: string
              VariantType: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene = get.Required.Raw Gene.Json.Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      VariantType = "variantType" |> flip get.Required.Field Decode.string }
                )


    module ``Somatic Biologically Relevant Variant`` =
        module Gene =
            /// Validate that a gene or a fusion gene is present and valid
            let validate geneJson fusionGeneJson =
              match Gene.Json.validate geneJson, FusionGene.validate fusionGeneJson with
              | (Ok gene, Ok fusionGene) -> Error $"Both gene and fusion gene json are valid: ({gene}, {fusionGene})"
              | (Error geneError, Error fusionError) -> Error $"Both gene and fusion gene are invalid: ({geneError}, {fusionError})"
              | (Ok gene, _) -> Ok <| Gene gene
              | (_, Ok fusionGene) -> Ok <| FusionGene fusionGene

        /// Represents a json object found in results.somaticBiologicallyRelevantVariants
        type Json =
            { GeneJson: Gene.Json
              FusionGene: FusionGene.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string
              VariantType: string
              VariantDescription: string
              StructuralVariant: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson   = Gene.Json.Decoder       |> get.Required.Raw
                      FusionGene = FusionGene.Json.Decoder |> get.Required.Raw
                      HgvsJson   = HGVS.Json.Decoder       |> get.Required.Raw
                      VariantType          = "variantType" |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                      StructuralVariant    = "structuralVariant"    |> flip get.Required.Field Decode.string
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string }
                )

        let validate json =
            json

    module ``Somatic Variant of Unknown Significance`` =
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

    module FusionVariant =
        type Json =
            { Gene5: Gene.Json
              Gene3: Gene.Json
              VariantDescription: string
              FusionType: string
              StructuralVariant: string option }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene5 = get.Required.Raw Gene.Json.Gene5Decoder
                      Gene3 = get.Required.Raw Gene.Json.Gene3Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      FusionType         = "fusionType"         |> flip get.Required.Field Decode.string
                      StructuralVariant  = "structuralVariant"  |> flip get.Required.Field Decoder.optionalString }
                )

    module InheritedRelevantVariant =
        type Json =
            { GeneJson: Gene.Json
              HgvsJson: HGVS.Json
              VariantDescription: string
              ClinicalSignificance: string
              Disease: string
              AllelicFraction: string
              Chromosome: int
              Ref: string
              Alt: string
              Pos: int }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                  { GeneJson = get.Required.Raw Gene.Json.Decoder
                    HgvsJson = get.Required.Raw HGVS.Json.Decoder
                    VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                    ClinicalSignificance = "clinicalSignificance" |> flip get.Required.Field Decode.string
                    Disease              = "disease"              |> flip get.Required.Field Decode.string
                    AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                    Chromosome           = "chromosome"           |> flip get.Required.Field Decode.int
                    Ref = "ref" |> flip get.Required.Field Decode.string
                    Alt = "alt" |> flip get.Required.Field Decode.string
                    Pos = "pos" |> flip get.Required.Field Decode.int }
                )

    /// The `results` section in the Tempus report
    module Results =
        type Json =
            { TumorMutationBurden: float option
              TumorMutationBurdenPercentile: int option
              MsiStatus: string option
              ``Somatic Potentially Actionable Mutations``: ``Somatic Potentially Actionable Mutation``.Json list
              ``Somatic Potentially Actionable Copy Number Variants``: ``Somatic Potentially Actionable Copy Number Variant``.Json list
              ``Somatic Biologically Relevant Variants``: ``Somatic Biologically Relevant Variant``.Json list
              ``Somatic Variants of Unknown Significance``: ``Somatic Variant of Unknown Significance``.Json list
              FusionVariants: FusionVariant.Json list
              InheritedRelevantVariants: InheritedRelevantVariant.Json list }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    // microsatellite instability status will either be a string field or an object that might have a 'Status' field
                    let msiObject = ["microsatelliteInstability"; "status"] |> flip get.Optional.At Decoder.optionalString // object with optional "status" field
                    let msiStringField = "msiStatus" |> flip get.Optional.Field Decoder.optionalString // field could be null, blank string, or actual value
                    let msiStatus = msiStringField |> Option.orElse msiObject |> Option.flatten

                    { TumorMutationBurden           = "tumorMutationalBurden"         |> flip get.Required.Field Decoder.optionalFloat // can either be a float, blank string (i.e. ""), or null
                      TumorMutationBurdenPercentile = "tumorMutationBurdenPercentile" |> flip get.Required.Field Decoder.optionalInteger // can either be an integer, blank string, or null
                      MsiStatus                     = msiStatus
                      ``Somatic Potentially Actionable Mutations`` = "somaticPotentiallyActionableMutations" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Mutation``.Json.Decoder)
                      ``Somatic Potentially Actionable Copy Number Variants`` = "somaticPotentiallyActionableCopyNumberVariants" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Copy Number Variant``.Json.Decoder)
                      ``Somatic Biologically Relevant Variants``   = "somaticBiologicallyRelevantVariants"  |> flip get.Required.Field (Decode.list ``Somatic Biologically Relevant Variant``.Json.Decoder)
                      ``Somatic Variants of Unknown Significance`` = "somaticVariantsOfUnknownSignificance" |> flip get.Required.Field (Decode.list ``Somatic Variant of Unknown Significance``.Json.Decoder)
                      FusionVariants = "fusionVariants" |> flip get.Required.Field (Decode.list FusionVariant.Json.Decoder)
                      InheritedRelevantVariants = ["inheritedRelevantVariants"; "values"] |> flip get.Required.At (Decode.list InheritedRelevantVariant.Json.Decoder)}
                )

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
                    Results = "results"   |> flip get.Required.Field Results.Json.Decoder }
              )

    module Json =
        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString Json.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })

