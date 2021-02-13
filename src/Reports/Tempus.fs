namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Utilities

    type Diagnosis =
        { DiagnosisName: DiagnosisName
          DiagnosisDate: DiagnosisDate option }
    and DiagnosisName = DiagnosisName of string
    and DiagnosisDate = DiagnosisDate of System.DateTime

    type TumorCategory = TumorCategory
    type GermlineCategory = GermlineCategory

    type Sample<'Category> =
        { SampleId: SampleId
          SampleSite: SampleSite
          SampleType: SampleType
          CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          TumorPercentage: TumorPercentage }
    and SampleId = SampleId of System.Guid
    and SampleSite = SampleSite of string
    and SampleType = SampleType of string
    and CollectionDate = CollectionDate of System.DateTime
    and ReceivedDate = ReceivedDate of System.DateTime
    and BlockId = BlockId of string
    and TumorPercentage = TumorPercentage of uint

    type Report =
        { Diagnosis: Diagnosis
          GermlineSample: Sample<GermlineCategory> option
          TumorSample: Sample<TumorCategory> }

    type Variant =
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
    and GeneName = internal GeneName of string
    and HgncId = internal HgncId of string
    and EntrezId = internal EntrezId of string

    and ``Somatic Biologically Relevant Gene`` =
        | Gene of Gene
        | FusionGene of FusionGene

    and FusionGene =
        { ``5' Gene``: Gene; ``3' Gene``: Gene }

    and HGVS =
        { ReferenceSequence: ReferenceSequence // also known as the transcript
          ``HGVS protein``: ``HGVS protein`` option
          ``HGVS change``: ``HGVS change`` }
    and ReferenceSequence = ReferenceSequence of string
    and VariantDescription = VariantDescription of string
    and VariantType = VariantType of string
    and MutationEffect = MutationEffect of string
    and ``HGVS protein`` =
        { // HGVS.pFull
          FullDescription: string 
          // HGVS.p
          NormalDescription: string }

    and ``HGVS change`` = ``HGVS change`` of string
    and NucleotideAlteration = NucleotideAlteration of string
    and AllelicFraction = AllelicFraction of float

    module Gene =
        open Thoth.Json.Net

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

    module HGVS =
        open Thoth.Json.Net

        // mutation effect will either be p or c
        // p will sometimes not be there
        type Json =
            { ``HGVS.p``: string
              ``HGVS.pFull``: string
              ``HGVS.c``: string
              Transcript: string // the reference sequence
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

    module ``Somatic Potentially Actionable Mutation`` =
        open Thoth.Json.Net

        type Json =
            { GeneJson: Gene.Json
              VariantJsons: VariantJson list }

            static member Decoder =
                Decode.object (fun get ->
                    { GeneJson = get.Required.Raw Gene.Json.Decoder
                      VariantJsons = "variants" |> flip get.Required.Field (Decode.list VariantJson.Decoder) }
                )

        and VariantJson =
            { HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string
              VariantDescription: string }

            static member Decoder : Decoder<VariantJson> =
                Decode.object (fun get ->
                    { HgvsJson = get.Required.Raw HGVS.Json.Decoder
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                    }
                )

    module ``Somatic Biologically Relevant Variant`` =
        open Thoth.Json.Net

        type Json =
            { GeneJson: Gene.Json
              Gene5Json: Gene.Json
              Gene3Json: Gene.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson  = Gene.Json.Decoder      |> get.Required.Raw
                      Gene5Json = Gene.Json.Gene5Decoder |> get.Required.Raw
                      Gene3Json = Gene.Json.Gene3Decoder |> get.Required.Raw
                      HgvsJson  = HGVS.Json.Decoder      |> get.Required.Raw
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string }
                )

    module ``Somatic Variant of Unknown Significance`` =
        open Thoth.Json.Net

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
        open Thoth.Json.Net

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

    /// The 'results' section in the Tempus report
    module Results =
        open Thoth.Json.Net

        type Json =
            { TumorMutationBurden: float option
              TumorMutationBurdenPercentile: int option
              MsiStatus: string option
              ``Somatic Potentially Actionable Mutations``: ``Somatic Potentially Actionable Mutation``.Json list
              ``Somatic Biologically Relevant Variants``: ``Somatic Biologically Relevant Variant``.Json list
              ``Somatic Variants of Unknown Significance``: ``Somatic Variant of Unknown Significance``.Json list
              FusionVariants: FusionVariant.Json list }

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
                      ``Somatic Biologically Relevant Variants``   = "somaticBiologicallyRelevantVariants"  |> flip get.Required.Field (Decode.list ``Somatic Biologically Relevant Variant``.Json.Decoder)
                      ``Somatic Variants of Unknown Significance`` = "somaticVariantsOfUnknownSignificance" |> flip get.Required.Field (Decode.list ``Somatic Variant of Unknown Significance``.Json.Decoder)
                      FusionVariants = "fusionVariants" |> flip get.Required.Field (Decode.list FusionVariant.Json.Decoder) }
                )

    module Json =
        open Thoth.Json.Net

        (* Json Definition *)

        type TempusJson =
            { Order: OrderJson
              Lab: LabJson
              Report: ReportJson
              Patient: PatientJson
              Samples: SampleJson list
              Results: Results.Json }

        and LabJson =
            { Name: string
              StreetAddress: string
              City: string
              State: string
              Zip: string
              CliaNumber: string }

        and ReportJson =
            { ReportId: System.Guid
              SigningPathologist: string
              SignoutDate: System.DateTime }

        and PatientJson =
            { FirstName: string
              LastName: string
              TempusId: System.Guid
              MRN: string
              Sex: string
              DateOfBirth: System.DateTime
              Diagnosis: string
              DiagnosisDate: string }

        and OrderJson =
            { Institution: string
              Physician: string
              OrderId: string
              AccessionId: string
              OrderTest: OrderTestJson }

        and OrderTestJson =
            { Code: string
              Name: string
              Description: string }

        and SampleJson =
            { SampleId: System.Guid
              CollectionDate: System.DateTime
              ReceivedDate: System.DateTime
              SampleCategory: string
              SampleSite: string
              SampleType: string
              Institution: InstitutionJson }

        and InstitutionJson =
            { BlockId: string option
              TumorPercentage: int option }


        (* Decoders *)

        type InstitutionJson with
            static member Decoder : Decoder<InstitutionJson> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.int }
                )

        type SampleJson with
            static member Decoder : Decoder<SampleJson> =
                Decode.object (fun get ->
                    { SampleId       = "tempusSampleId"  |> flip get.Required.Field Decode.guid
                      CollectionDate = "collectionDate"  |> flip get.Required.Field Decode.datetime
                      ReceivedDate   = "receiptDate"     |> flip get.Required.Field Decode.datetime
                      SampleCategory = "sampleCategory"  |> flip get.Required.Field Decode.string
                      SampleSite     = "sampleSite"      |> flip get.Required.Field Decode.string
                      SampleType     = "sampleType"      |> flip get.Required.Field Decode.string
                      Institution    = "institutionData" |> flip get.Required.Field InstitutionJson.Decoder })

        type OrderTestJson with
            static member Decoder : Decoder<OrderTestJson> =
                Decode.object (fun get ->
                    { Code        = "code" |> flip get.Required.Field Decode.string
                      Name        = "name" |> flip get.Required.Field Decode.string
                      Description = "description" |> flip get.Required.Field Decode.string }
                )

        type OrderJson with
            static member CamelCaseDecoder: Decoder<OrderJson> =
                Decode.object (fun get ->
                    { Institution = "institution"   |> flip get.Required.Field Decode.string
                      Physician   = "physician"     |> flip get.Required.Field Decode.string
                      OrderId     = "tempusOrderId" |> flip get.Required.Field Decode.string
                      AccessionId = "accessionId"   |> flip get.Required.Field Decode.string
                      OrderTest   = "test"          |> flip get.Required.Field OrderTestJson.Decoder }
                )

            static member SnakeCaseDecoder: Decoder<OrderJson> =
                Decode.object (fun get ->
                    { Institution = "institution"    |> flip get.Required.Field Decode.string
                      Physician   = "physician"      |> flip get.Required.Field Decode.string
                      OrderId     = "tempusOrder_id" |> flip get.Required.Field Decode.string
                      AccessionId = "accessionId"    |> flip get.Required.Field Decode.string
                      OrderTest   = "test"           |> flip get.Required.Field OrderTestJson.Decoder }
                )

            static member Decoder : Decoder<OrderJson> =
                Decode.oneOf [OrderJson.CamelCaseDecoder; OrderJson.SnakeCaseDecoder]

        type ReportJson with
            static member SnakeCaseDecoder : Decoder<ReportJson> =
                Decode.object (fun get ->
                    { ReportId           = "reportId"            |> flip get.Required.Field Decode.guid
                      SigningPathologist = "signing_pathologist" |> flip get.Required.Field Decode.string
                      SignoutDate        = "signout_date"        |> flip get.Required.Field Decode.datetime
                    }
                )

            static member CamelCaseDecoder : Decoder<ReportJson> =
                Decode.object (fun get ->
                    { ReportId           = "reportId"           |> flip get.Required.Field Decode.guid
                      SigningPathologist = "signingPathologist" |> flip get.Required.Field Decode.string
                      SignoutDate        = "signoutDate"        |> flip get.Required.Field Decode.datetime
                    }
                )

            /// Deserializer for the 'report' json object. Object attributes can be either snake-case or camel-case.
            static member Decoder : Decoder<ReportJson> =
               Decode.oneOf [ReportJson.CamelCaseDecoder; ReportJson.SnakeCaseDecoder]

        type LabJson with
            static member CamelCaseDecoder : Decoder<LabJson> =
                Decode.object (fun get ->
                    { Name          = "name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "streetAddress" |> flip get.Required.Field Decode.string
                      City          = "city"          |> flip get.Required.Field Decode.string
                      State         = "state"         |> flip get.Required.Field Decode.string
                      Zip           = "zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "cliaNo"        |> flip get.Required.Field Decode.string }
                )

            static member PascalCaseDecoder : Decoder<LabJson> =
                Decode.object (fun get ->
                    { Name          = "Name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "StreetAddress" |> flip get.Required.Field Decode.string
                      City          = "City"          |> flip get.Required.Field Decode.string
                      State         = "State"         |> flip get.Required.Field Decode.string
                      Zip           = "Zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "clia_no"       |> flip get.Required.Field Decode.string }
                )

            /// Deserializer for the 'lab' json object. Object attributes can be camel-cased or pascal-cased, save
            /// for the lab's clia #, which is camel-cased or snake-cased
            static member Decoder =
                Decode.oneOf [LabJson.CamelCaseDecoder; LabJson.PascalCaseDecoder]


        type PatientJson with
            static member CamelCaseDecoder : Decoder<PatientJson> =
                Decode.object (fun get ->
                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      MRN           = "emrId"     |> flip get.Required.Field Decode.string
                      Sex           = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = "dateOfBirth"   |> flip get.Required.Field Decode.datetime
                      Diagnosis     = "diagnosis"     |> flip get.Required.Field Decode.string
                      DiagnosisDate = "diagnosisDate" |> flip get.Required.Field Decode.string }
                )

            static member SnakeCaseDecoder : Decoder<PatientJson> =
                Decode.object (fun get ->
                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      MRN           = "emr_id"    |> flip get.Required.Field Decode.string
                      Sex           = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = "DoB"       |> flip get.Required.Field Decode.datetime
                      Diagnosis     = "diagnosis" |> flip get.Required.Field Decode.string
                      DiagnosisDate = "diagnosisDate" |> flip get.Required.Field Decode.string }
                )

            /// Deserializer for the 'patient' json object. The following object attributes can be camel-cased or snake-cased:
            /// - emrId / emr_id
            /// - dateOfBirth / DoB
            static member Decoder =
                Decode.oneOf [PatientJson.CamelCaseDecoder; PatientJson.SnakeCaseDecoder]

        type TempusJson with
            /// deserialize the json file as a whole: the lab, report, patient, order, and specimens object
            static member Decoder : Decoder<TempusJson> =
                Decode.object (fun get ->
                    { Lab     = "lab"       |> flip get.Required.Field LabJson.Decoder
                      Report  = "report"    |> flip get.Required.Field ReportJson.Decoder
                      Patient = "patient"   |> flip get.Required.Field PatientJson.Decoder
                      Order   = "order"     |> flip get.Required.Field OrderJson.Decoder
                      Samples = "specimens" |> flip get.Required.Field (Decode.list SampleJson.Decoder)
                      Results = "results"   |> flip get.Required.Field Results.Json.Decoder }
                )

        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString TempusJson.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })

