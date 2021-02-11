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
        { Gene: Gene
          HGVS: HGVS option
          AllelicFraction: AllelicFraction option
          NucleotideAlteration: NucleotideAlteration option }

    and HGVS =
        { ReferenceSequence: string }

    and Gene =
        { GeneName: GeneName
          HgncId: HgncId
          EntrezId: EntrezId }
    and GeneName = internal GeneName of string
    and HgncId = internal HgncId of string
    and EntrezId = internal EntrezId of string
    and VariantType = VariantType of string
    and VariantDescription = VariantDescription of string
    and MutationEffect = MutationEffect of string
    and ``HGVS protein`` = ``HGVS protein`` of string
    and ``HGVS protein full`` = ``HGVS protein full`` of string
    and ``HGVS change`` = ``HGVS change`` of string
    and Transcript = Transcript of string
    and NucleotideAlteration = NucleotideAlteration of string
    and AllelicFraction = AllelicFraction of float


    module Json =
        open Thoth.Json.Net

        (* Json Definition *)

        type TempusJson =
            { Order: OrderJson
              Lab: LabJson
              Report: ReportJson
              Patient: PatientJson
              Samples: SampleJson list }

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

        and ResultsJson =
            { TumorMutationBurden: string
              TumorMutationBurdenPercentile: string
              MsiStatus: string }

        and GeneJson =
            { GeneId: string // 'gene' attribute
              HgncId: string
              EntrezId: string }

        and HgvsJson =
            { ``HGVS.p``: string // same as mutation effect
              ``HGVS.pFull``: string
              ``HGVS.c``: string
              MutationEffect: string
              Transcript: string }

        and ``Somatic Potentially Actionable Mutation Json`` =
            { Gene: GeneJson
              Variants: ``Somatic Potentially Actionable Mutation Variant Json`` list }

        and ``Somatic Potentially Actionable Mutation Variant Json`` =
            { Hgvs: HgvsJson
              NucleotideAlteration: string
              AllelicFraction: string
              VariantDescription: string }

        and ``Somatic Potentially Actionable Copy Number Variant Json`` =
            { Gene: GeneJson
              VariantDescription: string
              VariantType: string  }

        and ``Somatic Biologically Relevant Variant Json`` =
            { Gene: GeneJson
              VariantType: string
              VariantDescription: string
              Hgvs: HgvsJson
              NucleotideAlteration: string
              AllelicFraction: string }

        and ``Somatic Variant Of Unknown Significance Json`` =
            { Gene: GeneJson
              VariantType: string
              VariantDescription: string
              Hgvs: HgvsJson
              NucleotideAlteration: string
              AllelicFraction: string }


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


        type ResultsJson with
            static member Decoder : Decoder<ResultsJson> =
                Decode.object (fun get ->
                    { TumorMutationBurden           = "tumorMutationalBurden"         |> flip get.Required.Field Decode.string
                      TumorMutationBurdenPercentile = "tumorMutationBurdenPercentile" |> flip get.Required.Field Decode.string
                      MsiStatus                     = "msiStatus"                     |> flip get.Required.Field Decode.string }
                )


        type TempusJson with
            /// deserialize the json file as a whole: the lab, report, patient, order, and specimens object
            static member Decoder : Decoder<TempusJson> =
                Decode.object (fun get ->
                    { Lab     = "lab"       |> flip get.Required.Field LabJson.Decoder
                      Report  = "report"    |> flip get.Required.Field ReportJson.Decoder
                      Patient = "patient"   |> flip get.Required.Field  PatientJson.Decoder
                      Order   = "order"     |> flip get.Required.Field OrderJson.Decoder
                      Samples = "specimens" |> flip get.Required.Field (Decode.list SampleJson.Decoder) }
                )

        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString TempusJson.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })