open System.IO
open System.Xml
open System.Xml.Schema
open System.Linq

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


let schemaGenerator = XmlSchemaGenerator()

schemaGenerator.AddXml("./data/Caris/TN20-111602_2020-11-12_00.27.xml")
schemaGenerator.AddXml("./data/Caris/TN15-115563_2020-11-13_21.26.xml")
schemaGenerator.AddXml("./data/Caris/TN20-111602_2020-11-12_00.27.xml")
schemaGenerator.AddXml("./data/Caris/TN20-116512_2020-11-11_15.17.xml")
schemaGenerator.AddXml("./data/Caris/TN20-139284_2020-11-11_15.42.xml")
schemaGenerator.AddXml("./data/Caris/TN20-153400_2020-11-12_00.10.xml")

// let schema = schemaGenerator.Schemas.First()
// let stringWriter = new StringWriter()
// schema.Write(stringWriter)



