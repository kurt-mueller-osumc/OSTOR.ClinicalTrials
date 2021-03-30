namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Thoth.Json.Net
    open Utilities

    /// the `lab` section of the Tempus report
    type Lab =
        { LabName: LabName
          CliaNumber: LabCliaNumber
          Address: Address }
    and LabName = LabName of string

    /// the `report` section of the Tempus report
    type Report =
        { ReportId: ReportId
          SigningPathologist: SigningPathologist
          SignoutDate: SignoutDate }
    and ReportId = ReportId of System.Guid
    and SigningPathologist = SigningPathologist of string
    and SignoutDate = SignoutDate of System.DateTime

    /// the `patient` section of the Tempus report
    type Patient =
        { MRN: MRN option
          FirstName: FirstName
          LastName: LastName
          TempusId: System.Guid
          Sex: Sex
          DateOfBirth: DateOfBirth
          DiagnosisName: Diagnosis.Name
          DiagnosisDate: DiagnosisDate option }
    and Sex = Male | Female
    and DiagnosisDate = internal DiagnosisDate of System.DateTime

    /// The `order` section of the Tempus report.
    type Order =
        { Institution: Institution
          Physician: Physician
          OrderId: OrderId
          AccessionId: AccessionId
          Test: OrderTest }
    and Institution = internal Institution of string
    and Physician = internal Physician of string // ordering md
    and OrderId = internal OrderId of string // different than the report id
    and AccessionId = internal AccessionId of string
    /// The `test` subsection of the `order` section in the Tempus report
    and OrderTest =
        { TestCode: TestCode
          TestName: TestName
          TestDescription: TestDescription }
    and TestCode = internal TestCode of string
    and TestName = internal TestName of string
    and TestDescription = internal TestDescription of string

    /// Each `sample` in the `samples` section of the Tempus report
    type Sample<'Category> =
        { SampleId: SampleId
          SampleSite: SampleSite
          SampleType: SampleType
          SampleDates: SampleDates
          BlockId: BlockId option
          TumorPercentage: TumorPercentage option }
    /// Tempus has either normal or tumor sample categories
    and TumorCategory = internal | TumorCategory
    and NormalCategory = internal | NormalCategory
    and TumorSample = Sample<TumorCategory>
    and NormalSample = Sample<NormalCategory>
    and SampleDates =
        { CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate}
    and CollectionDate = internal CollectionDate of System.DateTime
    and ReceivedDate = internal ReceivedDate of System.DateTime
    and SampleId = internal SampleId of System.Guid
    and SampleSite = internal SampleSite of string
    and SampleType =
        internal
        | Blood
        | ``FFPE Block``
        | ``FFPE Slides (Unstained)``
        | Saliva
    and BlockId = internal BlockId of string
    and TumorPercentage = internal TumorPercentage of uint

    type TumorMutationBurden =
        { Score: TumorMutationBurdenScore
          Percentile: TumorMutationBurdenPercentile }
    and TumorMutationBurdenScore        = internal TumorMutationBurdenScore of float
    and TumorMutationBurdenPercentile   = internal TumorMutationBurdenPercentile of uint
    and MicrosatelliteInstabilityStatus = internal MicrosatelliteInstabilityStatus of string

    type Gene =
        { GeneName: GeneName
          HgncId: HgncId
          EntrezId: EntrezId }
    and HgncId = internal HgncId of string
    and EntrezId = internal EntrezId of string

    /// a listing in the `somaticPotentiallyActionableMutations` subsection of the report's `results` section
    type ``Somatic Potentially Actionable Mutation`` =
        { Gene: Gene
          Variants: ``Somatic Potentially Actionable Variant`` list }
    /// the `variants` section of a `somaticPotentiallyActionableMutations` entry
    and ``Somatic Potentially Actionable Variant`` =
        { HGVS: HGVS
          NucleotideAlteration: NucleotideAlteration option
          AllelicFraction: AllelicFraction option
          VariantDescription: VariantDescription }

    /// a listing in the `somaticPotentiallyActionableCopyNumberVariants` subsection of the report's `results` section
    and ``Somatic Potentially Actionable Copy Number Variant`` =
        { Gene: Gene
          Description: CopyNumberVariantDescription
          Type: CopyNumberVariantType }
    and CopyNumberVariantDescription =
        internal
        | ``Copy number gain``
        | ``Copy number loss``
    and CopyNumberVariantType =
        internal
        | Amplification
        | Deletion

    and ``Somatic Biologically Relevant Variant`` =
        { Gene: ``Somatic Biologically Relevant Gene``
          HGVS: HGVS option
          AllelicFraction: AllelicFraction option
          NucleotideAlteration: NucleotideAlteration option
          Type: RelevantVariantType }

    and ``Somatic Biologically Relevant Gene`` =
        internal
        | RelevantGene of Gene
        | RelevantFusion of Fusion

    and Fusion =
        { ``5' Gene``: Gene
          ``3' Gene``: Gene
          FusionType: FusionType }

    and FusionType = internal FusionType of string

    and RelevantVariantType =
        internal
        | CNV
        | SNV
        | Fusion

    and HGVS =
        { ProteinChange: HgvsProteinChange option
          CodingChange: HgvsCodingChange
          ReferenceSequence: ReferenceSequence }

    and HgvsProteinChange =
        { AbbreviatedChange: HgvsAbbreviatedProteinChange
          FullChange: HgvsProteinFullChange }
    and HgvsAbbreviatedProteinChange = internal  HgvsAbbreviatedProteinChange of string
    and HgvsProteinFullChange = internal HgvsProteinFullChange of string
    and HgvsCodingChange = internal HgvsCodingChange of string
    and ReferenceSequence = internal ReferenceSequence of string
    and VariantDescription = internal VariantDescription of string
    and VariantType = internal VariantType of string
    and NucleotideAlteration = internal NucleotideAlteration of string
    and AllelicFraction = internal AllelicFraction of float

    type HgvsAbbreviatedProteinChange with member this.Value = this |> fun (HgvsAbbreviatedProteinChange proteinChange) -> proteinChange
    type HgvsProteinFullChange        with member this.Value = this |> fun (HgvsProteinFullChange proteinChange) -> proteinChange
    type HgvsCodingChange             with member this.Value = this |> fun (HgvsCodingChange codingChange) -> codingChange

    type Results =
        { TumorMutationBurden: TumorMutationBurden option
          MicrosatelliteInstabilityStatus: MicrosatelliteInstabilityStatus option
          ``Somatic Potentially Actionable Mutations``: ``Somatic Potentially Actionable Mutation`` list
          ``Somatic Potentially Actionable Copy Number Variants``: ``Somatic Potentially Actionable Copy Number Variant`` list }

    module Gene =
        /// Json object attributes that identifies genes
        type Json =
            { GeneId: string // 'gene' attribute
              HgncId: string
              EntrezId: string }

            /// Deserializes a gene's json object attributes
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene"     |> flip get.Required.Field Decode.string
                      HgncId   = "hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 5 json object attributes
            static member Gene5Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene5"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene5hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene5entrezId" |> flip get.Required.Field Decode.string })

            /// Deserializes Gene 3 json object attributes
            static member Gene3Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneId   = "gene3"         |> flip get.Required.Field Decode.string
                      HgncId   = "gene3hgncId"   |> flip get.Required.Field Decode.string
                      EntrezId = "gene3entrezId" |> flip get.Required.Field Decode.string })

        module Json =
            open StringValidations

            /// Validate that a json object representing a gene has a gene name, hgnc id, and entrez id
            let validate (json: Json) =
                match (json.GeneId, json.HgncId, json.EntrezId) with
                | (NotBlank, NotBlank, NotBlank) -> Ok { GeneName = GeneName json.GeneId; HgncId = HgncId json.HgncId; EntrezId = EntrezId json.EntrezId }
                | _ -> Error $"Gene missing name, hgnc id, or entrez id: {json}"

    module Fusion =
        type Json =
            { Gene5: Gene.Json
              Gene3: Gene.Json
              VariantDescription: string
              FusionType: string
              StructuralVariant: string option }

            /// Deserializer for a fusion of genes
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene5 = get.Required.Raw Gene.Json.Gene5Decoder
                      Gene3 = get.Required.Raw Gene.Json.Gene3Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      FusionType         = "fusionType"         |> flip get.Required.Field Decode.string
                      StructuralVariant  = "structuralVariant"  |> flip get.Required.Field Decoder.optionalString }
                )

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        module FusionType =
            /// Validate that a fusion type is not blank
            let validate =
                validateNotBlank
                >> Result.map FusionType
                >> Result.mapError (fun e -> $"Fusion type can't be blank: {e}")

        /// Validate that a fusion has 2 valid genes and a valid fusion type
        let validate (json: Json) =
            validation {
                let! gene5 = json.Gene5 |> Gene.Json.validate
                and! gene3 = json.Gene3 |> Gene.Json.validate
                and! fusionType = json.FusionType |> FusionType.validate

                return { ``5' Gene`` = gene5
                         ``3' Gene`` = gene3
                         FusionType = fusionType }
            }

    module HGVS =
        /// Represents HGVS reference sequences for proteins and coding DNA
        type Json =
            { ``HGVS.p``: string // abbreviated protein sequence change
              ``HGVS.pFull``: string // full protein sequence change
              ``HGVS.c``: string // coding DNA sequence change
              Transcript: string // the reference sequence (eg NM_012345.1)
              MutationEffect: string // will either equal 'HGVS.p' or 'HGVS.c'
            }

            /// Deserializer for hgvs json object attributes
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { ``HGVS.p``     = "HGVS.p"         |> flip get.Required.Field Decode.string
                      ``HGVS.pFull`` = "HGVS.pFull"     |> flip get.Required.Field Decode.string
                      ``HGVS.c``     = "HGVS.c"         |> flip get.Required.Field Decode.string
                      Transcript     = "transcript"     |> flip get.Required.Field Decode.string
                      MutationEffect = "mutationEffect" |> flip get.Required.Field Decode.string }
                )

        /// Retrieve the mutation effect for HGVS. If the protein change is present, report that. If not, report the coding change.
        let mutationEffect hgvs =
            match hgvs.ProteinChange with
            | Some proteinSequenceChange -> proteinSequenceChange.AbbreviatedChange.Value
            | _ -> hgvs.CodingChange.Value

        open Utilities.StringValidations

        /// Validate a HGVS reference sequence:
        ///
        /// 1. all hgvs fields are blank OR
        /// 2. if hgvs.c is present and hgvs.p is blank, mutationEffect == hgvs.c OR
        /// 3. if hgvs.p is present
        ///    - hgvs.pFull (vice versa) is present
        ///    - hgvs.p == mutation effect
        let validate json =
            match (json.``HGVS.p``, json.``HGVS.pFull``, json.``HGVS.c``, json.MutationEffect, json.Transcript) with
            /// No HGVS protein sequence change present; only a coding DNA sequence is present
            | (BlankString, BlankString, NotBlank, NotBlank, NotBlank) when json.``HGVS.c`` = json.MutationEffect ->
                Ok <| { ProteinChange = None
                        CodingChange = HgvsCodingChange json.``HGVS.c``
                        ReferenceSequence = ReferenceSequence json.Transcript }
            /// Both HGVS protein sequence change and coding DNA sequence change are present
            | (NotBlank, NotBlank, NotBlank, NotBlank, NotBlank) when json.``HGVS.p`` = json.MutationEffect ->
                Ok <| { ProteinChange = Some { AbbreviatedChange =  HgvsAbbreviatedProteinChange json.``HGVS.p``
                                               FullChange = HgvsProteinFullChange json.``HGVS.pFull`` }
                        CodingChange = HgvsCodingChange json.``HGVS.c``
                        ReferenceSequence = ReferenceSequence json.Transcript }
            | _ -> Error $"Invalid HGVS: {json}"

        /// If the HGVS is present, validate it.
        let validateOptional json =
            let hgvsMissing = [json.``HGVS.p``; json.``HGVS.pFull``; json.``HGVS.c``; json.MutationEffect; json.Transcript] |> List.forall String.isBlank

            if hgvsMissing then
                Ok None
            else
                validate json
                |> Result.map Some

    module Order =
        type Json =
            { Institution: string
              Physician: string
              OrderId: string
              AccessionId: string
              OrderTest: TestJson }

            /// Deserializer for the 'order' json object.
            ///
            ///  The following object attributes can be camel-cased or snake-cased:
            /// - `"tempusOrderId"` / `"tempusOrder_id"`
            static member Decoder: Decoder<Json> =
                Decode.object (fun get ->
                    let orderIdDecoder = ["tempusOrderId"; "tempusOrder_id"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf

                    { Institution = "institution"  |> flip get.Required.Field Decode.string
                      Physician   = "physician"    |> flip get.Required.Field Decode.string
                      OrderId     = orderIdDecoder |> get.Required.Raw
                      AccessionId = "accessionId"  |> flip get.Required.Field Decode.string
                      OrderTest   = "test"         |> flip get.Required.Field TestJson.Decoder }
                )

        and TestJson =
            { Code: string
              Name: string
              Description: string }

            /// Deserializer for the actual test code, name, and description ran on the report's patient
            static member Decoder : Decoder<TestJson> =
                Decode.object (fun get ->
                    { Code        = "code" |> flip get.Required.Field Decode.string
                      Name        = "name" |> flip get.Required.Field Decode.string
                      Description = "description" |> flip get.Required.Field Decode.string }
                )

        module Test =
            open Utilities.StringValidations

            /// Validate that a test's code, name, and description are not blank
            let validate (json: TestJson) =
                match (json.Code, json.Name, json.Description) with
                | NotBlank, NotBlank, NotBlank -> Ok <| { TestCode = TestCode json.Code; TestName = TestName json.Name; TestDescription = TestDescription json.Description }
                | _ -> Error $"Either test code, name, or description is blank: {json}"

        module OrderId =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that a tempus order id is in the following format where the first character is a 0, 1 or 2, followed by one digit, follwed by four letters: `"(0|1|2)dxxxx"`
            ///
            ///    validate (Input "20hnyc") = Ok (OrderId "20hnyc")
            ///    validate (Input "30aaaa") = Error "Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: 30aaaa"
            let validate (Input input) =
                if Regex("^(0|1|2){1}\d{1}[a-zA-z]{4}$").Match(input).Success then
                    Ok <| OrderId input
                else
                    Error $"Order id must match format (0|1|2)dxxxx where d is a digit and x is a letter: {input}"

        module AccessionId =
            open System.Text.RegularExpressions

            type Input = Input of string

            /// Validate that Tempus order's accession id is the in the following format where d is a digit and x is any alphanumeric character: `"TL-(0|1|2)d-xxxxxx"`
            ///
            ///    validate (Input "TL-19-DF60D1") = Ok (AccessionId "TL-19-DF60D1")
            //     validate (Input "TL-33-AAAAAA") = Error "Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: TL-33-AAAAAA"
            let validate (Input input) =
                if Regex("^TL-(0|1|2){1}\d{1}-(\d|[A-Z]|[a-z]){6}$").Match(input).Success then
                    Ok <| AccessionId input
                else
                    Error $"Accession id must be in the following format, TL-(0|1|2)d-xxxxxx: {input}"

        module Json =
            open Utilities.StringValidations
            open FsToolkit.ErrorHandling

            /// Validate the input from the "order" section of the json report
            let validate (json: Json) =
                validation {
                    let! institution = json.Institution |> validateNotBlank |> Result.map Institution
                    and! physician = json.Physician |> validateNotBlank |> Result.map Physician
                    and! orderId = json.OrderId |> OrderId.Input |> OrderId.validate
                    and! accessionId = json.AccessionId |> AccessionId.Input |> AccessionId.validate
                    and! orderTest = json.OrderTest |> Test.validate

                    return { Institution = institution
                             Physician = physician
                             OrderId = orderId
                             AccessionId = accessionId
                             Test = orderTest
                           } }

    module Lab =
        type Json =
            { Name: string
              StreetAddress: string
              City: string
              State: string
              Zip: string
              CliaNumber: string }

            static member CamelCaseDecoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Name          = "name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "streetAddress" |> flip get.Required.Field Decode.string
                      City          = "city"          |> flip get.Required.Field Decode.string
                      State         = "state"         |> flip get.Required.Field Decode.string
                      Zip           = "zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "cliaNo"        |> flip get.Required.Field Decode.string }
                )

            static member PascalCaseDecoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Name          = "Name"          |> flip get.Required.Field Decode.string
                      StreetAddress = "StreetAddress" |> flip get.Required.Field Decode.string
                      City          = "City"          |> flip get.Required.Field Decode.string
                      State         = "State"         |> flip get.Required.Field Decode.string
                      Zip           = "Zip"           |> flip get.Required.Field Decode.string
                      CliaNumber    = "clia_no"       |> flip get.Required.Field Decode.string }
                )

            /// Deserializer for the `lab` json object. Object attributes can be camel-cased or pascal-cased, save
            /// for the lab's clia #, which is camel-cased or snake-cased for some reason.
            static member Decoder : Decoder<Json> =
                Decode.oneOf [ Json.CamelCaseDecoder; Json.PascalCaseDecoder ]

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate the lab section of the json report
        let validate (json: Json) : Validation<Lab,string> =
            validation {
                let! labName = json.Name |> validateNotBlank |> Result.map LabName
                and! streetAddress = json.StreetAddress |> validateNotBlank |> Result.map StreetAddress
                and! city = json.City |> validateNotBlank |> Result.map City
                and! state = json.State |> validateNotBlank |> Result.map State
                and! zip = json.Zip |> validateNotBlank |> Result.map Zipcode
                and! cliaNumber = json.CliaNumber |> Lab.CliaNumber.Input |> Lab.CliaNumber.validate

                return { LabName = labName
                         Address = {
                             StreetAddress = streetAddress
                             City = city
                             State = state
                             Zipcode = zip
                         }
                         CliaNumber = cliaNumber
                       } }

    module Report =
        open System

        type Json =
            { ReportId: Guid
              SigningPathologist: string
              SignoutDate: DateTime }

            /// Deserializer for the "report" section of the Tempus report
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let pathologistDecoder = ["signing_pathologist"; "signingPathologist"] |> List.map (flip Decode.field Decode.string) |> Decode.oneOf
                    let signoutDateDecoder = ["signout_date"; "signoutDate"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { ReportId           = "reportId"         |> flip get.Required.Field Decode.guid
                      SigningPathologist = pathologistDecoder |> get.Required.Raw
                      SignoutDate        = signoutDateDecoder |> get.Required.Raw
                    } )

        open Utilities.StringValidations
        open FsToolkit.ErrorHandling

        /// Validate that a signing pathologist's name is not blank
        let validate (json: Json) : Validation<Report,string> =
            validation {
                let! signingPathologist = json.SigningPathologist |> validateNotBlank |> Result.map SigningPathologist

                return ({ ReportId = ReportId json.ReportId
                          SigningPathologist = signingPathologist
                          SignoutDate = SignoutDate json.SignoutDate} : Report
                       ) }

    module Sample =
        open System

        module SampleType =
            type Input = Input of string

            /// Validate that a sample type is either blood, block, slides, or saliva.
            let validate (Input input) =
                match input with
                | "Blood" -> Ok Blood
                | "FFPE Block" -> Ok ``FFPE Block``
                | "FFPE Slides (Unstained)" -> Ok ``FFPE Slides (Unstained)``
                | "Saliva" -> Ok Saliva
                | _ -> Error $"Invalid sample type: {input}"

        module CollectionDate =
            type Input = Input of DateTime

        module ReceivedDate =
            type Input = Input of DateTime

        module SampleDates =
            /// Validate that sample's collection date happens before its received date
            let validate (CollectionDate.Input collectionDate) (ReceivedDate.Input receivedDate) =
                if collectionDate < receivedDate then
                    Ok { CollectionDate = CollectionDate collectionDate; ReceivedDate = ReceivedDate receivedDate }
                else
                    Error $"Collection date, {collectionDate}, doesn't happen before received date, {receivedDate}."


        /// each entry in the `specimens` section of the Tempus report
        type Json =
            { SampleId: Guid
              CollectionDate: DateTime
              ReceivedDate: DateTime
              SampleCategory: string
              SampleSite: string
              SampleType: string
              Institution: InstitutionJson }

            /// Deserializer for the sample seciton of the Tempus report
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { SampleId       = "tempusSampleId"  |> flip get.Required.Field Decode.guid
                      CollectionDate = "collectionDate"  |> flip get.Required.Field Decode.datetime
                      ReceivedDate   = "receiptDate"     |> flip get.Required.Field Decode.datetime
                      SampleCategory = "sampleCategory"  |> flip get.Required.Field Decode.string
                      SampleSite     = "sampleSite"      |> flip get.Required.Field Decode.string
                      SampleType     = "sampleType"      |> flip get.Required.Field Decode.string
                      Institution    = "institutionData" |> flip get.Required.Field InstitutionJson.Decoder })

        and InstitutionJson =
            { BlockId: string option
              TumorPercentage: uint option }

            static member Decoder : Decoder<InstitutionJson> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.uint32
                    } )

    module TumorSample =
        module Category =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "tumor" -> Ok "tumor"
                | _ -> Error $"Sample category is not tumor: {input}"

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate a tumor sample
        let validate (json: Sample.Json) : Validation<TumorSample, string> =
            validation {
                // validate that the sample's listed category is 'tumor'
                let! tumorCategory = json.SampleCategory |> Category.Input |> Category.validate
                and! sampleDates   = (json.CollectionDate |> Sample.CollectionDate.Input, json.ReceivedDate |> Sample.ReceivedDate.Input) ||> Sample.SampleDates.validate
                and! sampleSite    = json.SampleSite |> validateNotBlank |> Result.map SampleSite
                and! sampleType    = json.SampleType |> Sample.SampleType.Input |> Sample.SampleType.validate

                return { SampleId = SampleId json.SampleId
                         SampleSite = sampleSite
                         SampleDates = sampleDates
                         SampleType = sampleType
                         BlockId = json.Institution.BlockId |> Option.map BlockId
                         TumorPercentage = json.Institution.TumorPercentage |> Option.map TumorPercentage
                       } }

    module NormalSample =
        module Category =
            type Input = Input of string

            let validate (Input input) =
                match input with
                | "normal" -> Ok "normal"
                | _ -> Error $"Sample category is not normal: {input}"

        open FsToolkit.ErrorHandling
        open Utilities.StringValidations

        /// Validate a normal sample
        let validate (json: Sample.Json) : Validation<NormalSample, string> =
            validation {
                // validate that the sample's listed category is 'nromal'
                let! normalCategory = json.SampleCategory |> Category.Input |> Category.validate
                and! sampleDates    = (json.CollectionDate |> Sample.CollectionDate.Input, json.ReceivedDate |> Sample.ReceivedDate.Input) ||> Sample.SampleDates.validate
                and! sampleSite     = json.SampleSite |> validateNotBlank |> Result.map SampleSite
                and! sampleType     = json.SampleType |> Sample.SampleType.Input |> Sample.SampleType.validate

                return { SampleId = SampleId json.SampleId
                         SampleSite = sampleSite
                         SampleDates = sampleDates
                         SampleType = sampleType
                         BlockId = json.Institution.BlockId |> Option.map BlockId
                         TumorPercentage = json.Institution.TumorPercentage |> Option.map TumorPercentage
                       } }

        /// Validate normal sample, if it's present
        let validateOptional (optionalJson: Sample.Json option) =
            match optionalJson with
            | None -> Ok None
            | Some json ->
                match validate json with
                | Ok normalSample -> Ok <| Some normalSample
                | Error e -> Error e

    module Patient =
        open System

        module Sex =
            type Input = Input of string

            /// Validate that sex is either `"(M|m)ale"` or `"(F|f)emale"`
            let validate (Input input) =
                match input with
                | "Male" | "male" -> Ok Male
                | "Female" | "female" -> Ok Female
                | _ -> Error $"Invalid sex: {input}"

        type Json =
            { FirstName: string
              LastName: string
              TempusId: Guid
              MrnJson: string option
              SexJson: string
              DateOfBirth: DateTime
              Diagnosis: string
              DiagnosisDate: DateTime option }

            /// Deserializer for the 'patient' json object.
            ///
            /// The following object attributes can be camel-cased or snake-cased:
            /// - `emrId` / `emr_id`
            /// - `dateOfBirth` / `DoB`
            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    let diagnosisDate = "diagnosisDate" |> flip get.Required.Field Decoder.optionalDateTime
                    let mrnDecoder = ["emrId"; "emr_id"] |> List.map (flip Decode.field Decoder.optionalString) |> Decode.oneOf
                    let dobDecoder = ["dateOfBirth"; "DoB"] |> List.map (flip Decode.field Decode.datetime) |> Decode.oneOf

                    { FirstName     = "firstName" |> flip get.Required.Field Decode.string
                      LastName      = "lastName"  |> flip get.Required.Field Decode.string
                      TempusId      = "tempusId"  |> flip get.Required.Field Decode.guid
                      MrnJson       = get.Required.Raw mrnDecoder
                      SexJson       = "sex"       |> flip get.Required.Field Decode.string
                      DateOfBirth   = get.Required.Raw dobDecoder
                      Diagnosis     = "diagnosis"   |> flip get.Required.Field Decode.string
                      DiagnosisDate = diagnosisDate }
                )

        open FsToolkit.ErrorHandling

        /// Validate a patient input: their name, mrn, and sex
        let validate (json: Json) =
            validation {
                let! firstName = json.FirstName |> FirstName.Input |> FirstName.validate
                and! lastName  = json.LastName  |> LastName.Input |> LastName.validate
                and! mrn       = json.MrnJson |> Option.map MRN.Input |> MRN.validateOptional
                and! sex       = json.SexJson |> Sex.Input |> Sex.validate

                return { MRN = mrn
                         FirstName = firstName
                         LastName = lastName
                         TempusId = json.TempusId
                         Sex = sex
                         DateOfBirth = json.DateOfBirth |> DateOfBirth
                         DiagnosisName = json.Diagnosis |> Diagnosis.Name
                         DiagnosisDate = json.DiagnosisDate |> Option.map DiagnosisDate
                       }}

    module Variant =
        module NucleotideAlteration =
            type Input = Input of string

            /// Validate that a nucleotide alteration is either none or something
            let validate input : Result<NucleotideAlteration option,string> =
                match input with
                | (Input "") -> Ok None
                | (Input na) -> Ok <| Some (NucleotideAlteration na)

            let validateOptional (optionalInput: Input option) : Result<NucleotideAlteration option, string> =
                match optionalInput with
                | Some input -> validate input
                | None -> Ok None

        module AllelicFraction =
            open Utilities.FloatValidations

            type Input = Input of string

            let (|ValidFraction|_|) (Input input) =
                input |> Float.tryParse

            /// Validate that an allelic fraction is either not present or is a valid, parseable float.
            let validate input =
                match input with
                | (Input "") -> Ok None
                | ValidFraction allelicFraction when allelicFraction >= 0.0 -> Ok <| Some (AllelicFraction allelicFraction)
                | _ -> Error $"Invalid allelic fraction: {input}"

            let validateOptional (optionalInput: Input option) =
                match optionalInput with
                | None -> Ok None
                | Some input -> validate input


        module Description =
            open StringValidations
            type Input = Input of string

            /// Validate that a variant description is not blank
            let validate (Input input) =
                input
                |> validateNotBlank
                |> Result.map VariantDescription
                |> Result.mapError (fun _ -> $"Variant description can't be blank: {input}")

    /// Logic for the `somaticPotentiallyActionableMutations` subsection of the report's `results` section
    module ``Somatic Potentially Actionable Mutation`` =
        module Variant =
            /// The json object for the `variants` section in each `somatic potentially actionable mutation`
            type Json =
                { HgvsJson: HGVS.Json
                  NucleotideAlteration: string
                  AllelicFraction: string
                  VariantDescription: string }

                static member Decoder : Decoder<Json> =
                    Decode.object (fun get ->
                        { HgvsJson = get.Required.Raw HGVS.Json.Decoder
                          NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                          AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                          VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                        } )

            open FsToolkit.ErrorHandling

            /// Validate a somatic, potentially actionable variant's hgvs, nucleotide alteration, allelic fraction, and variant description
            let validate (json: Json) : Validation<``Somatic Potentially Actionable Variant``,string> =
                validation {
                    let! hgvs = json.HgvsJson |> HGVS.validate
                    and! nucleotideAlteration = json.NucleotideAlteration |> Variant.NucleotideAlteration.Input |> Variant.NucleotideAlteration.validate
                    and! allelicFraction = json.AllelicFraction |> Variant.AllelicFraction.Input |> Variant.AllelicFraction.validate
                    and! variantDescription = json.VariantDescription |> Variant.Description.Input |> Variant.Description.validate

                    return { HGVS = hgvs
                             NucleotideAlteration = nucleotideAlteration
                             AllelicFraction = allelicFraction
                             VariantDescription = variantDescription
                           } }

        module Variants =
            open FsToolkit.ErrorHandling

            /// Validate a list of somatic, potentially actionable variants and return either a list of successfully validated variants or a list of errors
            let validate (json: Variant.Json list) =
                json
                |> Seq.map Variant.validate
                |> Seq.toList
                |> Result.combine
                |> Result.mapError List.flatten

        type Json =
            { GeneJson: Gene.Json
              VariantJsons: Variant.Json list }

            static member Decoder =
                Decode.object (fun get ->
                    { GeneJson = get.Required.Raw Gene.Json.Decoder
                      VariantJsons = "variants" |> flip get.Required.Field (Decode.list Variant.Json.Decoder)
                    })

        open FsToolkit.ErrorHandling

        let validate (json: Json) : Validation<``Somatic Potentially Actionable Mutation``, string> =
            validation {
                let! gene = json.GeneJson |> Gene.Json.validate
                and! variants = json.VariantJsons |> Variants.validate

                return { Gene = gene
                         Variants = variants } }

    module ``Somatic Potentially Actionable Mutations`` =
        open FsToolkit.ErrorHandling

        /// Validate a list of somatic, potentially actionable mutaitons
        let validate (jsons: ``Somatic Potentially Actionable Mutation``.Json list) =
            jsons
            |> Seq.map ``Somatic Potentially Actionable Mutation``.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten

    /// logic for `somaticPotentiallyActionableCopyNumberVariants` subsection of the `results` section
    module ``Somatic Potentially Actionable Copy Number Variant`` =
        module Description =
            type Input = Input of string

            /// Validate that a copy number variant description is either 'copy number gain' or 'copy number loss'
            let validate (Input input) =
                match input with
                | "Copy number gain" -> Ok ``Copy number gain``
                | "Copy number loss" -> Ok ``Copy number loss``
                | _ -> Error $"Invalid copy number variant description: {input}"

        module Type =
            type Input = Input of string

            /// Validate that a copy number variant type is either 'amplification' or 'deletion'
            let validate (Input input) =
                match input with
                | "amplification" -> Ok Amplification
                | "deletion" -> Ok Deletion
                | _ -> Error $"Invalid copy number variant type: {input}"

        type Json =
            { Gene: Gene.Json
              VariantDescription: string
              VariantType: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene = get.Required.Raw Gene.Json.Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      VariantType = "variantType" |> flip get.Required.Field Decode.string
                    } )

        open FsToolkit.ErrorHandling

        /// Validate a copy number variant input
        let validate (json: Json) =
            validation {
                let! gene = json.Gene |> Gene.Json.validate
                and! variantDescription = json.VariantDescription |> Description.Input |> Description.validate
                and! variantType = json.VariantType |> Type.Input |> Type.validate

                return { Gene = gene
                         Description = variantDescription
                         Type = variantType
                       } }

    module ``Somatic Potentially Actionable Copy Number Variants`` =
        open FsToolkit.ErrorHandling

        /// Validate a collection of somatic potentially actionable copy number variants
        let validate (jsons: ``Somatic Potentially Actionable Copy Number Variant``.Json list) =
            jsons
            |> Seq.map ``Somatic Potentially Actionable Copy Number Variant``.validate
            |> Seq.toList
            |> Result.combine
            |> Result.mapError List.flatten


    module ``Somatic Biologically Relevant Variant`` =
        module Gene =
            /// Validate that a gene or a fusion gene is present and valid
            let validate geneJson fusionGeneJson =
              match Gene.Json.validate geneJson, Fusion.validate fusionGeneJson with
              | (Ok gene, Ok fusionGene) -> Error $"Both gene and fusion gene json are valid: ({gene}, {fusionGene})"
              | (Error geneError, Error fusionError) -> Error $"Both gene and fusion gene are invalid: ({geneError}, {fusionError})"
              | (Ok gene, _) -> Ok <| RelevantGene gene
              | (_, Ok fusion) -> Ok <| RelevantFusion fusion

        module Type =
            type Input = Input of string

            /// Validate that a somatic biologically relevant variant type is either 'CNV', 'SNV', or 'fusion'
            let validate (Input input) =
                match input with
                | "CNV" -> Ok CNV
                | "SNV" -> Ok SNV
                | "fusion" -> Ok Fusion
                | _ -> Error $"Invalid somatic biologically relevant variant type: {input}"

        /// Represents a json object found in results.somaticBiologicallyRelevantVariants
        type Json =
            { GeneJson: Gene.Json
              FusionJson: Fusion.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string option
              AllelicFraction: string option
              VariantType: string
              VariantDescription: string
              StructuralVariant: string option }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson   = Gene.Json.Decoder   |> get.Required.Raw
                      FusionJson = Fusion.Json.Decoder |> get.Required.Raw
                      HgvsJson   = HGVS.Json.Decoder   |> get.Required.Raw
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                      StructuralVariant    = "structuralVariant"    |> flip get.Required.Field Decoder.optionalString
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decoder.optionalString
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decoder.optionalString
                    } )

        open FsToolkit.ErrorHandling

        /// Validate that a somatic, biologically relevant variant has either a gene or a fusion of genese, hgvs, a variant type of cnv, snv, or fusion, and valid nucleotide laterations and allelic fractions.
        let validate (json: Json) =
            validation {
                let! fusionOrGene = (json.GeneJson, json.FusionJson) ||> Gene.validate
                and! hgvs = json.HgvsJson |> HGVS.validateOptional
                and! variantType = json.VariantType |> Type.Input |> Type.validate
                and! nucleotideAlteration = json.NucleotideAlteration |> Option.map Variant.NucleotideAlteration.Input |> Variant.NucleotideAlteration.validateOptional
                and! allelicFraction = json.AllelicFraction |> Option.map Variant.AllelicFraction.Input |> Variant.AllelicFraction.validateOptional

                return { Gene = fusionOrGene
                         HGVS = hgvs
                         AllelicFraction = allelicFraction
                         NucleotideAlteration = nucleotideAlteration
                         Type = variantType
                } }

    module ``Somatic Variant of Unknown Significance`` =
        type Json =
            { GeneJson: Gene.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string
              VariantType: string
              VariantDescription: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson = Gene.Json.Decoder |> get.Required.Raw
                      HgvsJson = HGVS.Json.Decoder |> get.Required.Raw
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                      VariantType          = "variantType"          |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string }
                )

    module InheritedRelevantVariant =
        type Json =
            { GeneJson: Gene.Json
              HgvsJson: HGVS.Json
              VariantDescription: string
              ClinicalSignificance: string
              Disease: string
              AllelicFraction: string
              Chromosome: int
              Ref: string
              Alt: string
              Pos: int }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                  { GeneJson = get.Required.Raw Gene.Json.Decoder
                    HgvsJson = get.Required.Raw HGVS.Json.Decoder
                    VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                    ClinicalSignificance = "clinicalSignificance" |> flip get.Required.Field Decode.string
                    Disease              = "disease"              |> flip get.Required.Field Decode.string
                    AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string
                    Chromosome           = "chromosome"           |> flip get.Required.Field Decode.int
                    Ref = "ref" |> flip get.Required.Field Decode.string
                    Alt = "alt" |> flip get.Required.Field Decode.string
                    Pos = "pos" |> flip get.Required.Field Decode.int }
                )

    /// The `results` section in the Tempus report
    module Results =
        module TumorMutationBurden =
            type ScoreInput = ScoreInput of float
            type PercentileInput = PercentileInput of int

            /// Validate that tmb score and percentile are both present or absent.
            ///
            ///    validateOptional None None = Ok None
            ///    validateOptional (Some (ScoreInput 1.0)) (Some (PercentileInput 34)) = Ok
            let validateOptional (scoreInput: ScoreInput option) (percentileInput: PercentileInput option) =
                match scoreInput, percentileInput with
                | Some (ScoreInput score), Some (PercentileInput percentile) -> Ok <| Some { Score = TumorMutationBurdenScore score; Percentile = TumorMutationBurdenPercentile (uint percentile) }
                | None, None -> Ok None
                | Some _, None -> Error $"TMB percentile is missing: {scoreInput}"
                | None, Some _ -> Error $"TMB score is missing: {scoreInput}"

        module MicrosatelliteInstabilityStatus =
            type Input = Input of string

            /// Validate that msi status, if present, contains at least one character
            let validateOptional (input: Input option) =
                match input with
                | None -> Ok None
                | Some (Input status) when String.isNotBlank status -> Ok <| Some (MicrosatelliteInstabilityStatus status)
                | Some _ -> Error $"MSI status can't be blank"

        type Json =
            { TumorMutationBurden: float option
              TumorMutationBurdenPercentile: int option
              MsiStatus: string option
              ``Somatic Potentially Actionable Mutations``: ``Somatic Potentially Actionable Mutation``.Json list
              ``Somatic Potentially Actionable Copy Number Variants``: ``Somatic Potentially Actionable Copy Number Variant``.Json list
              ``Somatic Biologically Relevant Variants``: ``Somatic Biologically Relevant Variant``.Json list
              ``Somatic Variants of Unknown Significance``: ``Somatic Variant of Unknown Significance``.Json list
              Fusions: Fusion.Json list
              InheritedRelevantVariants: InheritedRelevantVariant.Json list }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    // microsatellite instability status will either be a string field or an object that might have a 'Status' field
                    let msiObject = ["microsatelliteInstability"; "status"] |> flip get.Optional.At Decoder.optionalString // object with optional "status" field
                    let msiStringField = "msiStatus" |> flip get.Optional.Field Decoder.optionalString // field could be null, blank string, or actual value
                    let msiStatus = msiStringField |> Option.orElse msiObject |> Option.flatten

                    { TumorMutationBurden           = "tumorMutationalBurden"         |> flip get.Required.Field Decoder.optionalFloat // can either be a float, blank string (i.e. ""), or null
                      TumorMutationBurdenPercentile = "tumorMutationBurdenPercentile" |> flip get.Required.Field Decoder.optionalInteger // can either be an integer, blank string, or null
                      MsiStatus                     = msiStatus
                      ``Somatic Potentially Actionable Mutations`` = "somaticPotentiallyActionableMutations" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Mutation``.Json.Decoder)
                      ``Somatic Potentially Actionable Copy Number Variants`` = "somaticPotentiallyActionableCopyNumberVariants" |> flip get.Required.Field (Decode.list ``Somatic Potentially Actionable Copy Number Variant``.Json.Decoder)
                      ``Somatic Biologically Relevant Variants``   = "somaticBiologicallyRelevantVariants"  |> flip get.Required.Field (Decode.list ``Somatic Biologically Relevant Variant``.Json.Decoder)
                      ``Somatic Variants of Unknown Significance`` = "somaticVariantsOfUnknownSignificance" |> flip get.Required.Field (Decode.list ``Somatic Variant of Unknown Significance``.Json.Decoder)
                      Fusions = "fusionVariants" |> flip get.Required.Field (Decode.list Fusion.Json.Decoder)
                      InheritedRelevantVariants = ["inheritedRelevantVariants"; "values"] |> flip get.Required.At (Decode.list InheritedRelevantVariant.Json.Decoder)
                    } )

        open FsToolkit.ErrorHandling

        /// Validate the `results` section of the json report
        let validate (json: Json) =
            validation {
                let! tmb = (json.TumorMutationBurden           |> Option.map TumorMutationBurden.ScoreInput,
                            json.TumorMutationBurdenPercentile |> Option.map TumorMutationBurden.PercentileInput) ||> TumorMutationBurden.validateOptional
                and! msiStatus = json.MsiStatus |> Option.map MicrosatelliteInstabilityStatus.Input |> MicrosatelliteInstabilityStatus.validateOptional
                and! somaticPotentiallyActionableMutations = json.``Somatic Potentially Actionable Mutations`` |> ``Somatic Potentially Actionable Mutations``.validate
                and! somaticPotentiallyActionableCopyNumberVariants = json.``Somatic Potentially Actionable Copy Number Variants`` |> ``Somatic Potentially Actionable Copy Number Variants``.validate

                return { TumorMutationBurden = tmb
                         MicrosatelliteInstabilityStatus = msiStatus
                         ``Somatic Potentially Actionable Mutations`` = somaticPotentiallyActionableMutations
                         ``Somatic Potentially Actionable Copy Number Variants`` = somaticPotentiallyActionableCopyNumberVariants
                       } }

    type Json =
        { Order: Order.Json
          Lab: Lab.Json
          Report: Report.Json
          Patient: Patient.Json
          Samples: Sample.Json list
          Results: Results.Json }

          /// deserialize the json file as a whole: the lab, report, patient, order, and specimens object
          static member Decoder : Decoder<Json> =
              Decode.object (fun get ->
                  { Lab     = "lab"       |> flip get.Required.Field Lab.Json.Decoder
                    Report  = "report"    |> flip get.Required.Field Report.Json.Decoder
                    Patient = "patient"   |> flip get.Required.Field Patient.Json.Decoder
                    Order   = "order"     |> flip get.Required.Field Order.Json.Decoder
                    Samples = "specimens" |> flip get.Required.Field (Decode.list Sample.Json.Decoder)
                    Results = "results"   |> flip get.Required.Field Results.Json.Decoder
                  } )

            member this.TumorSample =
                this.Samples |> Seq.find (fun sample -> sample.SampleCategory = "tumor")

            member this.TryNormalSample =
                this.Samples |> Seq.tryFind (fun sample -> sample.SampleCategory = "normal")

    type OverallReport =
        { Lab: Lab
          Patient: Patient
          Report: Report
          Order: Order
          Results: Results
          TumorSample: TumorSample
          NormalSample: NormalSample option
        }

    module Json =
        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString Json.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })

        open FsToolkit.ErrorHandling

        /// Validate an overall report
        let validate (json: Json) =
            validation {
                let! lab     = json.Lab     |> Lab.validate
                and! report  = json.Report  |> Report.validate
                and! patient = json.Patient |> Patient.validate
                and! order   = json.Order   |> Order.Json.validate
                and! tumorSample  = json.TumorSample     |> TumorSample.validate
                and! normalSample = json.TryNormalSample |> NormalSample.validateOptional
                and! results = json.Results |> Results.validate

                return { Lab     = lab
                         Report  = report
                         Patient = patient
                         Order   = order
                         TumorSample  = tumorSample
                         NormalSample = normalSample
                         Results = results
                       } }

    module Database =
        open Database

        let toPatientRow (json: Json) =
            let row = context.Public.Patients.Create()
            let patient = json.Patient

            // row.Mrn <- patient.MRN.Value

            row

