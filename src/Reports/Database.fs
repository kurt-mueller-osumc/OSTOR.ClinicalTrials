namespace OSTOR.ClinicalTrials.Reports

module Database =
    open FSharp.Data.Sql

    [<Literal>]
    let dbVendor = Common.DatabaseProviderTypes.POSTGRESQL
    [<Literal>]
    let connString = "Host=host.docker.internal;Database=ostor_development;Username=postgres;Passfile=" + __SOURCE_DIRECTORY__ + "/data/.pgpass"
    [<Literal>]
    let npgsqlPath = @"/home/vscode/.nuget/packages/npgsql/5.0.3/lib/net5.0"

    type SQL = SqlDataProvider<dbVendor, connString, ResolutionPath=npgsqlPath, UseOptionTypes=true>

    let context = SQL.GetDataContext()

    let querySampleReportId (reportId: string) (sampleId: string) =
        query {
            for sampleReport in context.Public.SampleReports do
            where (sampleReport.ReportId = reportId && sampleReport.SampleId = sampleId)
            select sampleReport.Id
            exactlyOne
        }

    module DTO =
        open System
        open Core

        /// A dto representing a row in the `vendors` table
        type Vendor =
            { CreatedAt: DateTime
              Lab: Lab }

            member this.Row =
                let row = context.Public.Vendors.Create()
                let lab = this.Lab

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt
                row.Name <- lab.Name.Value
                row.CliaNumber <- lab.CliaNumber.Value
                row.StreetAddress <- lab.Address.Street.Value
                row.City <- lab.Address.City.Value
                row.State <- lab.Address.State.Value
                row.ZipCode <- lab.Address.Zip.Value

                row

        module Vendor =
            /// Build a row to be inserted into `vendors` table
            let buildRow (dto: Vendor) = dto.Row

        /// A dto representing a row in the `patients` table
        type Patient =
            { CreatedAt: DateTime
              MRN: Patient.MRN
              FirstName: Person.FirstName
              LastName: Person.LastName
              DateOfBirth: Person.DateOfBirth
              Sex: string }

            member this.Row =
                let row = context.Public.Patients.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt
                row.Mrn <- this.MRN.Value
                row.FirstName <- this.FirstName.Value
                row.LastName <- this.LastName.Value
                row.DateOfBirth <- this.DateOfBirth.Value

                row

        module Patient =
            /// Build a row to be inserted into `patients` table
            let buildRow (dto: Patient) = dto.Row

        /// A dto representing a row in the `genes` table
        type Gene =
            { CreatedAt: DateTime
              Name: Gene.Name
              EntrezId: int option
              HgncId: string option }

            /// Build a row to be inserted into `genes` table
            member this.Row =
                let row = context.Public.Genes.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt

                row.Name <- this.Name.Value
                row.EntrezId <- this.EntrezId
                row.HgncId <- this.HgncId

                row

        module Gene =
            let buildRow (dto: Gene) = dto.Row

        /// A dto representing a row in the `samples` table
        type Sample =
            { CreatedAt: DateTime
              SampleId: string
              SampleType: string
              Category: string
              BiopsySite: string option }

            /// Build a row to be inserted into `samples` table
            member this.Row =
                let row = context.Public.Samples.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt

                row.SampleId <- this.SampleId
                row.SampleType <- this.SampleType
                row.Category <- this.Category
                row.BiopsySite <- this.BiopsySite

                row

        module Sample =
            let buildRow (dto: Sample) = dto.Row

        type Report =
            { // meta
              CreatedAt: DateTime
              ReportId: string
              IssuedDate: DateTime
              // foreign keys
              PatientMRN: Patient.MRN
              VendorCliaNumber: CliaNumber
              // diagnosis
              DiagnosisName: Diagnosis.Name
              DiagnosisDate: DateTime option
              DiagnosisIcdCodes: string list
              // ordering physician
              OrderingPhysicianName: string option
              OrderingPhysicianNumber: NationalProviderId option
              Pathologist: string option
              // biomarkers
              MicrosatelliteInstabilityStatus: string option
              TumorMutationBurden: float option
              TumorMutationBurdenPercentile: int option
            }

            member this.Row =
                let row = context.Public.Reports.Create()

                // meta
                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt
                row.ReportId <- this.ReportId
                row.IssuedDate <- this.IssuedDate

                // foreign keys
                row.PatientMrn <- this.PatientMRN.Value
                row.VendorCliaNumber <- this.VendorCliaNumber.Value

                // diagnosis
                row.DiagnosisName <- this.DiagnosisName.Value
                row.DiagnosisDate <- this.DiagnosisDate
                row.DiagnosisIcd10Codes <- this.DiagnosisIcdCodes |> List.toArray |> Some

                // ordering physician
                row.OrderingPhysician <- this.OrderingPhysicianName
                row.OrderingPhysicianNumber <- this.OrderingPhysicianNumber |> Option.map (fun npi -> npi.Value)
                row.Pathologist <- this.Pathologist

                // biomarkers (tumor mutation burden, microsatellite instability)
                row.TumorMutationalBurden <- this.TumorMutationBurden
                row.TumorMutationalBurdenPercentile <- this.TumorMutationBurdenPercentile
                row.MsiStatus <- this.MicrosatelliteInstabilityStatus

                row

        type SampleReport =
            { CreatedAt: DateTime
              // foreign keys
              SampleId: string
              ReportId: string
              // identifier
              BlockId: Sample.BlockId option
              // dates
              CollectionDate: Sample.CollectionDate option
              ReceivedDate: Sample.ReceivedDate
              TumorPercentage: int option }

            member this.Row =
                let row = context.Public.SampleReports.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt

                row.SampleId <- this.SampleId
                row.ReportId <- this.ReportId
                row.BlockId <- this.BlockId |> Option.map (fun blockId -> blockId.Value)

                row.CollectionDate <- this.CollectionDate |> Option.map (fun collectionDate -> collectionDate.Value)
                row.ReceiptDate <- this.ReceivedDate.Value

                row.TumorPercentage <- this.TumorPercentage

                row

        type Variant =
            { CreatedAt: DateTime
              // foreign keys
              GeneName: Gene.Name
              SampleReportId: Guid
              // identifier
              Name: string
              Category: Variant.Category
              // opinions
              Type: string option
              Assessment: string option
              // info
              Description: string option
              AllelicFraction: float option

              // HGVS
              Transcript: string option
              HgvsCodingChange: string option
              HgvsProteinFullChange: string option
              HgvsProteinAbbreviatedChange: string option
              NucleotideAlteration: string option
            }

            member this.Row =
                let row = context.Public.Variants.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt

                // foreign keys
                row.GeneName <- this.GeneName.Value
                row.SampleReportId <- this.SampleReportId

                // identifier
                row.Name <- this.Name
                row.Category <- this.Category.Value

                // opinions
                row.Type <- this.Type
                row.Assessment <- this.Assessment

                // info
                row.Description <- this.Description
                row.AllelicFraction <- this.AllelicFraction

                // hgvs
                row.Transcript <- this.Transcript
                row.HgvsC <- this.HgvsCodingChange
                row.HgvsProteinFull <- this.HgvsProteinFullChange
                row.HgvsProtein <- this.HgvsProteinAbbreviatedChange
                row.NucleotideAlteration <- this.NucleotideAlteration

                row


        type Fusion =
            { CreatedAt: DateTime
              // foreign keys
              Gene1Name: Gene.Name
              Gene2Name: Gene.Name
              SampleReportId: Guid
              // info
              Description: Fusion.Description option
              Type: string
            }

            member this.Row =
                let row = context.Public.Fusions.Create()

                row.CreatedAt <- this.CreatedAt
                row.UpdatedAt <- this.CreatedAt

                row.FirstGeneName <- this.Gene1Name.Value
                row.SecondGeneName <- this.Gene2Name.Value
                row.SampleReportId <- this.SampleReportId
                row.Description <- this.Description |> Option.map (fun description -> description.Value)
                row.FusionType <- this.Type

                row
