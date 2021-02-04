namespace OSTOR.ClinicalTrials.Reports

module Utilities =
    module DateTime =
        let tryParse(input: string) =
            match System.DateTime.TryParse(input) with
            | (true, dateTime) -> Some dateTime
            | _ -> None

    module List =
        /// Flatten list of lists into just a 1-d list
        ///
        ///    flatten [[1]; [2; 3; 4]; [5; 6]] = [1; 2; 3; 4; 5; 6]
        let flatten list =
            List.collect id list

    module Regex =
        open System.Text.RegularExpressions

        /// Remove a regex expression from a string
        let remove regex string =
            Regex.Replace(string, regex, "")

    module Result =
        /// Combine a list of results into a result of lists
        let combine (results: Result<'ok, 'err> list) : Result<'ok list, 'err list> =
            let initial : Result<'ok list, 'err list> = Ok []

            results
            |> List.fold (fun agg res ->
                match res with
                | Ok ok -> Result.map (fun oks -> ok :: oks) agg
                | Error err -> Result.mapError (fun errs -> err :: errs) agg
            ) initial

    module String =
        open System.IO

        let split char (string: string) =
            string.Split([| char |]) |> Array.toList

        let readFromDisk path = File.ReadAllText(path)

        let writeToDisk path string =
            File.WriteAllText(path, string)

    module Xml =
        open System.IO
        open System.Linq
        open System.Xml
        open System.Xml.Schema

        type XmlSchemaGenerator() =
            let mutable schemaSet = XmlSchemaSet()
            let schemaInference = XmlSchemaInference()

            member this.AddXml (path: string) =
                use xmlReader = XmlReader.Create(path)

                schemaSet <- schemaInference.InferSchema(xmlReader, schemaSet)
                schemaSet.Compile()
                this

            member _.Schemas = Enumerable.Cast<XmlSchema>(schemaSet.Schemas())

            member this.Schema = this.Schemas.First()

            member this.SchemaString =
                let stringWriter = new StringWriter()
                this.Schema.Write(stringWriter)
                stringWriter.ToString()

            member this.SchemaStrings =
                this.Schemas |> Seq.map (fun schema ->
                    let stringWriter = new StringWriter()
                    schema.Write(stringWriter)
                    stringWriter.ToString()
                )

            member this.SaveSchemaToDisk path =
                File.WriteAllText(path, this.SchemaString)

        module SchemaGenerator =
            let addXml xmlPath (schemaGenerator: XmlSchemaGenerator) =
                schemaGenerator.AddXml xmlPath

            let schemaString (schemaGenerator: XmlSchemaGenerator) =
                schemaGenerator.SchemaString
