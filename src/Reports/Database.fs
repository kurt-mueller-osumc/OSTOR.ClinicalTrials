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

    module DTO =
        open Core

        /// A dto representing a row in the `vendors` table
        type Vendor =
            { CreatedAt: System.DateTime
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
            { CreatedAt: System.DateTime
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
            { CreatedAt: System.DateTime
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
            { CreatedAt: System.DateTime
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
              CreatedAt: System.DateTime
              ReportId: string
              IssuedDate: System.DateTime
              // foreign keys
              PatientMRN: Patient.MRN
              VendorCliaNumber: CliaNumber
              // diagnosis
              DiagnosisName: Diagnosis.Name
              DiagnosisDate: System.DateTime option
              DiagnosisIcdCodes: string list
              // ordering physician
              OrderingPhysicanName: string option
              OrderingPhysicanNumber: int64 option
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
                row.OrderingPhysician <- this.OrderingPhysicanName
                row.OrderingPhysicianNumber <- this.OrderingPhysicanNumber

                // biomarkers (tumor mutation burden, microsatellite instability)
                row.TumorMutationalBurden <- this.TumorMutationBurden
                row.TumorMutationalBurdenPercentile <- this.TumorMutationBurdenPercentile
                row.MsiStatus <- this.MicrosatelliteInstabilityStatus

                row