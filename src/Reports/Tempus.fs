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
          Report: ReportJson
          Patient: PatientJson }

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


    (* Decoders *)

    type OrderTestJson with
        static member Decoder : Decoder<OrderTestJson> =
            Decode.object (fun get ->
                { Code = get.Required.Field "code" Decode.string
                  Name = get.Required.Field "name" Decode.string
                  Description = get.Required.Field "description" Decode.string }
            )

    type OrderJson with
        static member Decoder : Decoder<OrderJson> =
            Decode.object (fun get ->
                { Institution = get.Required.Field "institution" Decode.string
                  Physician = get.Required.Field "physician" Decode.string
                  OrderId = get.Required.Field "tempusOrder_id" Decode.string
                  AccessionId = get.Required.Field "accessionId" Decode.string
                  OrderTest = get.Required.Field "test" OrderTestJson.Decoder }
            )

    type ReportJson with
        static member SnakeCaseDecoder : Decoder<ReportJson> =
            Decode.object (fun get ->
                { ReportId = get.Required.Field "reportId" Decode.guid
                  SigningPathologist = get.Required.Field "signing_pathologist" Decode.string
                  SignoutDate = get.Required.Field "signout_date" Decode.datetime
                }
            )

        static member CamelCaseDecoder : Decoder<ReportJson> =
            Decode.object (fun get ->
                { ReportId = get.Required.Field "reportId" Decode.guid
                  SigningPathologist = get.Required.Field "signingPathologist" Decode.string
                  SignoutDate = get.Required.Field "signoutDate" Decode.datetime
                }
            )


    type LabJson with
        static member PascalCaseDecoder : Decoder<LabJson> =
            Decode.object (fun get ->
                { Name = get.Required.Field "Name" Decode.string
                  StreetAddress = get.Required.Field "StreetAddress" Decode.string
                  City = get.Required.Field "City" Decode.string
                  State = get.Required.Field "State" Decode.string
                  Zip = get.Required.Field "Zip" Decode.string
                  CliaNumber = get.Required.Field "clia_no" Decode.string }
            )

        static member CamelCaseDecoder : Decoder<LabJson> =
            Decode.object (fun get ->
                { Name = get.Required.Field "name" Decode.string
                  StreetAddress = get.Required.Field "streetAddress" Decode.string
                  City = get.Required.Field "city" Decode.string
                  State = get.Required.Field "state" Decode.string
                  Zip = get.Required.Field "zip" Decode.string
                  CliaNumber = get.Required.Field "cliaNo" Decode.string }
            )

    type PatientJson with
        static member Decoder : Decoder<PatientJson> =
            Decode.object (fun get ->
                { FirstName = get.Required.Field "firstName" Decode.string
                  LastName = get.Required.Field "lastName" Decode.string
                  TempusId = get.Required.Field "tempusId" Decode.guid
                  MRN = get.Required.Field "emr_id" Decode.string
                  Sex = get.Required.Field "sex" Decode.string
                  DateOfBirth = get.Required.Field "DoB" Decode.datetime
                  Diagnosis = get.Required.Field "diagnosis" Decode.string
                  DiagnosisDate = get.Required.Field "diagnosisDate" Decode.string }
            )

    type Json with
        static member Decoder : Decoder<Json> =
            Decode.object (fun get ->
                { Lab = get.Required.Field "lab" (Decode.oneOf [LabJson.PascalCaseDecoder; LabJson.CamelCaseDecoder])
                  Report = get.Required.Field "report" (Decode.oneOf [ReportJson.CamelCaseDecoder; ReportJson.SnakeCaseDecoder])
                  Patient = get.Required.Field "patient" PatientJson.Decoder
                  Order = get.Required.Field "order" OrderJson.Decoder }
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