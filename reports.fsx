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

let tempusReportJsonsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Tempus"|])

// deserialize tempus jsons
let tempusJsonResults =
    DirectoryInfo(tempusReportJsonsPath).EnumerateFileSystemInfos("*.json")
    |> Seq.map (fun filePath ->
        let jsonText = File.ReadAllText(filePath.FullName)
        Tempus.Json.deserializeWithError filePath.Name jsonText
    ) |> Seq.toList

let (tempusJsons, tempusJsonErrors) = Result.partition tempusJsonResults

// validate reports from tempus jsons
let (tempusReports, tempusErrors) =
    tempusJsons
    |> Seq.map Tempus.Json.validate
    |> Seq.toList
    |> Result.partition

// insert valid tempus reports into database
tempusReports
|> List.sortByDescending (fun tempusReport -> tempusReport.Report.ReportId)
|> List.choose Tempus.DTO.tryCreate
|> List.map (fun dto ->
    printfn $"Insert tempus: {dto.Report}"
    dto.Insert
)

(*
    FMI Reports
*)

let fmiReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "FMI"|])

printfn "FMI: Read and validate reports"

let (fmiReports, fmiErrors) =
    DirectoryInfo(fmiReportsPath).EnumerateFileSystemInfos("*.xml")
    |> Seq.map (fun filePath ->
        printfn $"FMI XML: {filePath}"
        FoundationMedicine.XML.Report(filePath.FullName).Report |> FoundationMedicine.Input.Report.validate
    )
    |> Seq.toList
    |> Result.partition

printfn "FMI: Convert valid reports to DTOs & insert them into database"

// insert validate FMI reports into database
fmiReports
|> List.sortByDescending (fun report -> report.ReportId)
|> List.choose (fun report ->
    try
        printfn $"FMI report: {report.ReportId}"
        FoundationMedicine.DTO.tryCreate report
    with
        | :? NullReferenceException ->
            printfn "Caught null reference"
            printfn $"{report.PMI}"
            raise (NullReferenceException())

)
|> List.map (fun dto ->
    printfn $"Insert FMI: {dto.Report}"
    dto.Insert
)


(*
    Caris Reports
*)

let carisReportsPath = Path.Combine([| Environment.CurrentDirectory; "data"; "Caris"|])

// read in and validate caris reports
let (carisReports, carisErrors) =
    DirectoryInfo(carisReportsPath).EnumerateFileSystemInfos("*.xml")
    |> Seq.map (fun filePath ->
        Caris.Xml.Report(filePath.FullName).Report
        |> Caris.Input.Report.validate
    )
    |> Seq.toList
    |> Result.partition

// insert caris reports into the database
carisReports
|> List.sortByDescending (fun carisReport -> carisReport.Test.ReceivedDate)
|> List.choose Caris.DTO.tryCreate
|> List.map (fun dto ->
    printfn $"Insert {dto.Report}"
    dto.Insert
)
