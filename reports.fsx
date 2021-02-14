#r "nuget: Thoth.Json.Net"
#r "src/Reports/bin/Debug/net5.0/Reports.dll"

open System
open System.IO

open OSTOR.ClinicalTrials.Reports
open Utilities


(* Tempus Reports *)

let tempusReportJsonsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Tempus"|])

let jsonResults =
    DirectoryInfo(tempusReportJsonsPath).EnumerateFileSystemInfos("*.json")
    |> Seq.map (fun filePath ->
        let jsonText = File.ReadAllText(filePath.FullName)
        Tempus.Json.deserializeWithError filePath.Name jsonText
    ) |> Seq.toList

let (jsons, errors) = Result.partition jsonResults
let json = Seq.head jsons
