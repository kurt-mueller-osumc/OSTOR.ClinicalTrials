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

        module Vendor =
            /// Build a row to be inserted into `vendors` table
            let buildRow (dto: Vendor) =
                let row = context.Public.Vendors.Create()
                let lab = dto.Lab

                row.CreatedAt <- dto.CreatedAt
                row.Name <- lab.Name.Value
                row.CliaNumber <- lab.CliaNumber.Value
                row.StreetAddress <- lab.Address.Street.Value
                row.City <- lab.Address.City.Value
                row.State <- lab.Address.State.Value
                row.ZipCode <- lab.Address.Zip.Value

                row

        /// A dto representing a row in the `patients` table
        type Patient =
            { CreatedAt: System.DateTime
              MRN: Patient.MRN
              FirstName: Person.FirstName
              LastName: Person.LastName
              DateOfBirth: Person.DateOfBirth
              Sex: string }

        module Patient =
            /// Build a row to be inserted into `patients` table
            let buildRow (dto: Patient) =
                let row = context.Public.Patients.Create()

                row.CreatedAt <- dto.CreatedAt
                row.Mrn <- dto.MRN.Value
                row.FirstName <- dto.FirstName.Value
                row.LastName <- dto.LastName.Value
                row.DateOfBirth <- dto.DateOfBirth.Value

                row