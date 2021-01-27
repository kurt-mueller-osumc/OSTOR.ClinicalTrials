#r "nuget: FSharp.Data"
#r "./src/Reports/bin/Debug/net5.0/Reports.dll"

open System.IO
open System.Xml.Linq

open FSharp.Data

[<Literal>]
let fmiResultsReportPath     = __SOURCE_DIRECTORY__ + "/src/Reports/data/fmiReport0.xsd"

[<Literal>]
let fmiClinicalReportXsdPath = __SOURCE_DIRECTORY__ + "/src/Reports/data/fmiReport1.xsd"

[<Literal>]
let fmiVariantReportXsdPath = __SOURCE_DIRECTORY__ + "/src/Reports/data/fmiReport2.xsd"

type FmiClinicalReportXsd = XmlProvider<Schema = fmiClinicalReportXsdPath>
type FmiVariantReportXsd  = XmlProvider<Schema = fmiVariantReportXsdPath>


[<Literal>]
let fmiXmlsPath = "./data/FMI/ORD-0758611-01.xml"
let xmlText = File.ReadAllText(fmiXmlsPath)

let rrNs = XNamespace.Get "http://integration.foundationmedicine.com/reporting"
let vrNs = XNamespace.Get "http://foundationmedicine.com/compbio/variant-report-external"
let xml = XDocument.Parse(xmlText)

let finalReportElement = xml.Element(rrNs + "ResultsReport")
                            .Element(rrNs + "ResultsPayload")
                            .Element(XName.Get "FinalReport")
                            .ToString()

let fmiClinicalReport = FmiClinicalReportXsd.Parse(finalReportElement)

let variantReportElement = xml.Element(rrNs + "ResultsReport")
                              .Element(rrNs + "ResultsPayload")
                              .Element(vrNs + "variant-report")
                              .ToString()

let fmiVariantReport = FmiVariantReportXsd.Parse(variantReportElement)
