namespace OSTOR.ClinicalTrials.Reports

module Utilities =
    module Regex =
        open System.Text.RegularExpressions

        /// Remove a regex expression from a string
        let remove regex string =
            Regex.Replace(string, regex, "")

    module String =
        open System.IO

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
                let xmlReader = XmlReader.Create(path)

                schemaSet <- schemaInference.InferSchema(xmlReader, schemaSet)
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