namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Thoth.Json.Net

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

    type Json =
        { Order: OrderJson
          Lab: LabJson
          Report: ReportJson }

        static member Decoder : Decoder<Json> =
            Decode.object (fun get ->
                { Order = get.Required.Field "order" OrderJson.Decoder
                  Lab = get.Required.Field "lab" LabJson.Decoder
                  Report = get.Required.Field "report" ReportJson.Decoder }
            )

    and LabJson =
        { Name: string
          StreetAddress: string
          City: string
          State: string
          Zip: string
          CliaNumber: string }

        static member Decoder : Decoder<LabJson> =
            Decode.object (fun get ->
                { Name = get.Required.Field "name" Decode.string
                  StreetAddress = get.Required.Field "StreetAddress" Decode.string
                  City = get.Required.Field "City" Decode.string
                  State = get.Required.Field "State" Decode.string
                  Zip = get.Required.Field "zip" Decode.string
                  CliaNumber = get.Required.Field "clia_no" Decode.string }
            )

    and ReportJson =
        { ReportId: System.Guid
          SigningPathologist: string
          SignoutDate: string }

        static member Decoder : Decoder<ReportJson> =
            Decode.object (fun get ->
                { ReportId = get.Required.Field "reportId" Decode.guid
                  SigningPathologist = get.Required.Field "signing_pathologist" Decode.string
                  SignoutDate = get.Required.Field "signout_date" Decode.string
                }
            )

    and OrderJson =
        { Institution: string
          Physician: string
          OrderId: string
          AccessionId: string
          OrderTest: OrderTestJson }

        static member Decoder : Decoder<OrderJson> =
            Decode.object (fun get ->
                { Institution = get.Required.Field "institution" Decode.string
                  Physician = get.Required.Field "physician" Decode.string
                  OrderId = get.Required.Field "tempusOrder_id" Decode.string
                  AccessionId = get.Required.Field "accessionId" Decode.string
                  OrderTest = get.Required.Field "test" OrderTestJson.Decoder }
            )

    and OrderTestJson =
        { Code: string
          Name: string
          Description: string }

        static member Decoder : Decoder<OrderTestJson> =
            Decode.object (fun get ->
                { Code = get.Required.Field "code" Decode.string
                  Name = get.Required.Field "name" Decode.string
                  Description = get.Required.Field "description" Decode.string }
            )

    module Json =
        let deserialize =
            Decode.fromString Json.Decoder


    // let foo =
      // Decode.

    // type Json =
    //     { Lab: LabJson
    //       Report: ReportJson
    //       Patient: PatientJson
    //       Order: OrderJson
    //       Samples: SampleJson list }
    // and LabJson =
    //     { Name: string
    //       StreetAddress: string
    //       City: string
    //       State: string
    //       Zip: string
    //       CliaNumber: string }
    // and ReportJson =
    //     { ReportId: string
    //       SigningPathologist: string option
    //       SignoutDate: System.DateTime option }
    // and PatientJson =
    //     { FirstName: string
    //       LastName: string
    //       TempusId: string
    //       MRN: string
    //       Sex: string
    //       DateOfBirth: System.DateTime
    //       Diagnosis: string
    //       DiagnosisDate: string }
    // and OrderJson =
    //     { Institution: string
    //       Physician: string
    //       OrderId: string
    //       AccessionId: string
    //       OrderTest: OrderTestJson }
    // and OrderTestJson =
    //     { Code: string
    //       Name: string
    //       Description: string }
    // and SampleJson =
    //     { SampleId: string
    //       CollectionDate: string
    //       ReceivedDate: string
    //       SampleCategory: string
    //       SampleSite: string
    //       SampleType: string
    //       Notes: string
    //       InstitutionData: InstitutionJson }
    // and InstitutionJson =
    //     { CaseId: string
    //       BlockId: string option
    //       TumorPercentage: int option }

    //       static member Decoder : Decoder<InstitutionJson> =
    //         Decode.object (fun get ->
    //           { CaseId = get.Required.Field "caseId" Decode.string
    //             BlockId = get.Optional.Field "blockId" Decode.string
    //             TumorPercentage = get.Optional.Field "tumorPercentage" Decode.int }
    //         )


    // module Json =
        // let deserialize jsonText =
            // Json.deserialize<Json>(jsonText)