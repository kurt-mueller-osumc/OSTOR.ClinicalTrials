namespace OSTOR.ClinicalTrials.Reports

module Utilities =
    module Xml =
        open System.IO
        open System.Linq
        open System.Xml
        open System.Xml.Schema

        type XmlSchemaGenerator(schemaSet: XmlSchemaSet, schemaInference: XmlSchemaInference) =
            let mutable schemaSet = schemaSet
            let schemaInference = schemaInference

            new() = XmlSchemaGenerator(XmlSchemaSet(), XmlSchemaInference())

            member _.AddXml (path: string) =
                let xmlReader = XmlReader.Create(path)

                schemaSet <- schemaInference.InferSchema(xmlReader, schemaSet)

            member _.Schemas =
                Enumerable.Cast<XmlSchema>(schemaSet.Schemas())

            member this.Schema =
                this.Schemas.First()

            member this.SchemaString =
                let stringWriter = new StringWriter()
                this.Schema.Write(stringWriter)
                stringWriter.ToString()