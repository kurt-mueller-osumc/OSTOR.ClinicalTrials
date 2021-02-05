namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Newtonsoft.Json

    type Json =
        { [<JsonProperty("lab")>] Lab: LabJson
          [<JsonProperty("report")>] Report: ReportJson }
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

    module Json =
        let deserialize jsonText =
            JsonConvert.DeserializeObject<Json>(jsonText)