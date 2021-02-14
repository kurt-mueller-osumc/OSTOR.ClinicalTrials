#r "nuget: Thoth.Json.Net"
#r "nuget: FSharp.Data"
#r "nuget: FsToolkit.ErrorHandling"

#r "src/Reports/bin/Debug/net5.0/Reports.dll"

open System
open System.IO

open FsToolkit.ErrorHandling

open OSTOR.ClinicalTrials.Reports
open Utilities


(* Tempus Reports *)

let tempusReportJsonsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Tempus"|])

let tempusJsonResults =
    DirectoryInfo(tempusReportJsonsPath).EnumerateFileSystemInfos("*.json")
    |> Seq.map (fun filePath ->
        let jsonText = File.ReadAllText(filePath.FullName)
        Tempus.Json.deserializeWithError filePath.Name jsonText
    ) |> Seq.toList

let (tempusJsons, tempusErrors) = Result.partition tempusJsonResults


(* FMI Reports *)


let fmiReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "FMI"|])

let fmiReportResults =
    DirectoryInfo(fmiReportsPath).EnumerateFileSystemInfos("*.xml")
    |> Seq.map (fun filePath -> FoundationMedicine.Report.Xml(filePath.FullName).ReportInput |> FoundationMedicine.Report.validate)
    |> Seq.toList


let (fmiReports, fmiErrors) = Result.partition fmiReportResults