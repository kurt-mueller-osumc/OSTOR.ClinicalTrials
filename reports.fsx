#r "nuget: Thoth.Json.Net"
#r "nuget: FSharp.Data"
#r "nuget: SQLProvider"
#r "nuget: FsToolkit.ErrorHandling"

#r "src/Reports/bin/Debug/net5.0/Reports.dll"

open System
open System.IO

open FsToolkit.ErrorHandling

open OSTOR.ClinicalTrials.Reports
open Utilities

(* Tempus Reports *)

// let tempusReportJsonsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Tempus"|])

// let tempusJsonResults =
//     DirectoryInfo(tempusReportJsonsPath).EnumerateFileSystemInfos("*.json")
//     |> Seq.map (fun filePath ->
//         let jsonText = File.ReadAllText(filePath.FullName)
//         Tempus.Json.deserializeWithError filePath.Name jsonText
//     ) |> Seq.toList

// let (tempusJsons, tempusJsonErrors) = Result.partition tempusJsonResults


// let (tempusReports, tempusErrors) =
//     tempusJsons
//     |> Seq.map (fun json ->
//         printfn $"Tempus report id: {json.Report.ReportId}"
//         Tempus.Json.validate json
//     )
//     |> Seq.toList
//     |> Result.partition


(* FMI Reports *)

// let fmiReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "FMI"|])

// let (fmiReports, fmiErrors) =
//     DirectoryInfo(fmiReportsPath).EnumerateFileSystemInfos("*.xml")
//     // |> Seq.map (fun filePath -> FoundationMedicine.XML.Report(filePath.FullName))
//     |> Seq.map (fun filePath ->
//         printfn $"FMI XML: {filePath}"
//         FoundationMedicine.XML.Report(filePath.FullName).Report |> FoundationMedicine.Input.Report.validate
//     )
//     |> Seq.toList
//     |> Result.partition


// let (fmiReports, fmiErrors) = Result.partition fmiResults


// let fmiResults =
//     DirectoryInfo(fmiReportsPath).EnumerateFileSystemInfos("*.xml")
//     // |> Seq.map (fun filePath -> FoundationMedicine.XML.Report(filePath.FullName))
//     |> Seq.map (fun filePath ->
//         printfn $"Reading {filePath}"
//         FoundationMedicine.XML.Report(filePath.FullName).Report |> FoundationMedicine.Input.Report.validate
//     )
//     |> Seq.toList


// let (fmiReports, fmiErrors) = Result.partition fmiResults


// (* Caris Reports *)

let carisReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Caris"|])

let (carisReports, carisErrors) =
    DirectoryInfo(carisReportsPath).EnumerateFileSystemInfos("*.xml")
    |> Seq.map (fun filePath ->
        printfn $"Caris report: {filePath}"

        Caris.Xml.Report(filePath.FullName).Report
        |> Caris.Input.Report.validate

    )
    |> Seq.toList
    |> Result.partition


carisReports
|> List.sortByDescending (fun carisReport -> carisReport.Test.ReceivedDate)
|> List.choose Caris.DTO.tryCreate
|> List.map (fun dto ->
    printfn $"Insert {dto.Report}"
    dto.Insert
)
