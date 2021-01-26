namespace OSTOR.ClinicalTrials.Reports

module Utilities =
    module Regex =
        open System.Text.RegularExpressions

        /// Remove a regex expression from a string
        let remove regex string =
            Regex.Replace(string, regex, "")

    module Xml =
        open System.IO
        open System.Linq
        open System.Xml
        open System.Xml.Schema

        type XmlSchemaGenerator() =
            let mutable schemaSet = XmlSchemaSet()
            let schemaInference = XmlSchemaInference()

            member _.AddXml (path: string) =
                let xmlReader = XmlReader.Create(path)

                schemaSet <- schemaInference.InferSchema(xmlReader, schemaSet)

            member _.Schemas = Enumerable.Cast<XmlSchema>(schemaSet.Schemas())

            member this.Schema = this.Schemas.First()

            member this.SchemaString =
                let stringWriter = new StringWriter()
                this.Schema.Write(stringWriter)
                stringWriter.ToString()

            member this.SaveSchemaToDisk path =
                File.WriteAllText(path, this.SchemaString)