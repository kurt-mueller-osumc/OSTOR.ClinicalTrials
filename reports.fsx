#r "nuget: Thoth.Json.Net"
#r "nuget: FSharp.Data"
#r "nuget: FsToolkit.ErrorHandling"

#r "src/Reports/bin/Debug/net5.0/Reports.dll"

open System
open System.IO

open FsToolkit.ErrorHandling

open OSTOR.ClinicalTrials.Reports

(* Tempus Reports *)

// let tempusReportJsonsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Tempus"|])

// let tempusJsonResults =
//     DirectoryInfo(tempusReportJsonsPath).EnumerateFileSystemInfos("*.json")
//     |> Seq.map (fun filePath ->
//         let jsonText = File.ReadAllText(filePath.FullName)
//         Tempus.Json.deserializeWithError filePath.Name jsonText
//     ) |> Seq.toList

// let (tempusJsons, tempusErrors) = Result.partition tempusJsonResults


(* FMI Reports *)

let fmiReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "FMI"|])

let (fmiReports, fmiErrors) =
    DirectoryInfo(fmiReportsPath).EnumerateFileSystemInfos("*.xml")
    // |> Seq.map (fun filePath -> FoundationMedicine.XML.Report(filePath.FullName))
    |> Seq.map (fun filePath ->
        printfn $"FMI XML: {filePath}"
        FoundationMedicine.XML.Report(filePath.FullName).Report |> FoundationMedicine.Input.Report.validate
    )
    |> Seq.toList
    |> Result.partition


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


// // (* Caris Reports *)

// let carisReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Caris"|])

// let carisXmls =
//     DirectoryInfo(carisReportsPath).EnumerateFileSystemInfos("*.xml")
//     |> Seq.map (fun filePath -> Caris.Report.Xml(filePath.FullName))
//     |> Seq.toList

