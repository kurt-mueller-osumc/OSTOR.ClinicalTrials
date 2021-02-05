namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Newtonsoft.Json

    type Diagnosis =
        { DiagnosisName: DiagnosisName
          DiagnosisDate: DiagnosisDate option }
    and DiagnosisName = DiagnosisName of string
    and DiagnosisDate = DiagnosisDate of System.DateTime

    type Json =
        { [<JsonProperty("lab")>] Lab: LabJson
          [<JsonProperty("report")>] Report: ReportJson
          [<JsonProperty("patient")>] Patient: PatientJson
          [<JsonProperty("order")>] Order: OrderJson }
    and LabJson =
        { Name: string
          StreetAddress: string
          City: string
          State: string
          Zip: string
          [<JsonProperty("clia_no")>] CliaNumber: string }
    and ReportJson =
        { [<JsonProperty("reportId")>] ReportId: string
          [<JsonProperty("signing_pathologist")>] SigningPathologist: string
          [<JsonProperty("signout_date")>] SignoutDate: string }
    and PatientJson =
        { [<JsonProperty("firstName")>] FirstName: string
          [<JsonProperty("lastName")>] LastName: string
          [<JsonProperty("tempusId")>] TempusId: string
          [<JsonProperty("emr_id")>] MRN: string
          [<JsonProperty("sex")>] Sex: string
          [<JsonProperty("DoB")>] DateOfBirth: string
          [<JsonProperty("diagnosis")>] Diagnosis: string
          [<JsonProperty("diagnosisDate")>] DiagnosisDate: string }
    and OrderJson =
        { [<JsonProperty("institution")>] Institution: string
          [<JsonProperty("physician")>] Physician: string
          [<JsonProperty("tempusOrder_id")>] OrderId: string
          [<JsonProperty("accessionId")>] AccessionId: string
          [<JsonProperty("test")>] OrderTest: OrderTestJson }
    and OrderTestJson =
        { [<JsonProperty("code")>] Code: string
          [<JsonProperty("name")>] Name: string
          [<JsonProperty("description")>] Description: string }

    module Json =
        let deserialize jsonText =
            JsonConvert.DeserializeObject<Json>(jsonText)