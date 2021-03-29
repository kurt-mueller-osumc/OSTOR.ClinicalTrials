namespace OSTOR.ClinicalTrials.Reports

module Tempus =
    open Thoth.Json.Net
    open Utilities

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

    /// Tempus has either normal or tumor sample categories
    type TumorCategory = internal | TumorCategory
    type NormalCategory = internal | NormalCategory

    type Sample<'Category> =
        { SampleId: SampleId
          SampleSite: SampleSite
          SampleType: SampleType
          CollectionDate: CollectionDate
          ReceivedDate: ReceivedDate
          BlockId: BlockId
          TumorPercentage: TumorPercentage }
    and SampleId = internal SampleId of System.Guid
    and SampleSite = internal SampleSite of string
    and SampleType = internal SampleType of string
    and CollectionDate = internal CollectionDate of System.DateTime
    and ReceivedDate = internal ReceivedDate of System.DateTime
    and BlockId = internal BlockId of string
    and TumorPercentage = internal TumorPercentage of uint

    type Lab =
        { LabName: LabName
          CliaNumber: LabCliaNumber
          Address: Address }
    and LabName = LabName of string

    /// The "report" section of the Tempus report. Each report has a diagnosis, a tumor sample, and an optional normal sample
    type Report =
        { Diagnosis: Diagnosis
          NormalSample: Sample<NormalCategory> option
          TumorSample: Sample<TumorCategory> }
    and Diagnosis =
        { DiagnosisName: Diagnosis.Name
          DiagnosisDate: DiagnosisDate option }

    /// The "order" section of the Tempus report.
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
    /// The "test" subsection residing in the "order" section.
    and OrderTest =
        { TestCode: TestCode
          TestName: TestName
          TestDescription: TestDescription }
    and TestCode = internal TestCode of string
    and TestName = internal TestName of string
    and TestDescription = internal TestDescription of string

    type TumorMutationBurden =
        { Score: TumorMutationBurdenScore
          Percentile: TumorMutationBurdenPercentile }
    and TumorMutationBurdenScore        = internal TumorMutationBurdenScore of float
    and TumorMutationBurdenPercentile   = internal TumorMutationBurdenPercentile of uint
    and MicrosatelliteInstabilityStatus = internal MicrosatelliteInstabilityStatus of string

    type Variant =
        internal
        | ``Somatic Biologically Relevant Variant`` of ``Somatic Biologically Relevant Variant``

    and ``Somatic Biologically Relevant Variant`` =
        { Gene: ``Somatic Biologically Relevant Gene``
          HGVS: HGVS option
          AllelicFraction: AllelicFraction option
          NucleotideAlteration: NucleotideAlteration option }

    and Gene =
        { GeneName: GeneName
          HgncId: HgncId
          EntrezId: EntrezId }
    and HgncId = internal HgncId of string
    and EntrezId = internal EntrezId of string

    and ``Somatic Biologically Relevant Gene`` =
        internal
        | Gene of Gene
        | Fusion of Fusion

    and Fusion =
        { ``5' Gene``: Gene
          ``3' Gene``: Gene
          FusionType: FusionType }
    and FusionType = internal FusionType of string

    and HGVS =
        { ``Protein Sequence Change``: ``HGVS Protein Sequence Change`` option
          ``Coding DNA Sequence Change``: ``HGVS Coding DNA Sequence Change``
          ReferenceSequence: ReferenceSequence }

    and ``HGVS Protein Sequence Change`` =
        { AbbreviatedChange: ``Abbreviated Protein Sequence Change``
          FullChange: ``Full Protein Sequence Change`` }
    and ``Abbreviated Protein Sequence Change`` = internal ``Abbreviated Protein Sequence Change`` of string
    and ``Full Protein Sequence Change`` = internal ``Full Protein Sequence Change`` of string
    and ``HGVS Coding DNA Sequence Change`` = internal ``HGVS Coding DNA Sequence Change`` of string
    and ReferenceSequence = internal ReferenceSequence of string
    and VariantDescription = internal VariantDescription of string
    and VariantType = internal VariantType of string
    and NucleotideAlteration = internal NucleotideAlteration of string
    and AllelicFraction = internal AllelicFraction of float

    type ``Abbreviated Protein Sequence Change`` with
        member this.Value = this |> fun (``Abbreviated Protein Sequence Change`` proteinChange) -> proteinChange
    type ``Full Protein Sequence Change`` with
        member this.Value = this |> fun (``Full Protein Sequence Change`` proteinChange) -> proteinChange
    type ``HGVS Coding DNA Sequence Change`` with
        member this.Value = this |> fun (``HGVS Coding DNA Sequence Change`` codingChange) -> codingChange

    type Results =
        { TumorMutationBurden: TumorMutationBurden option
          MicrosatelliteInstabilityStatus: MicrosatelliteInstabilityStatus option }

    type OverallReport =
        { Lab: Lab
          Patient: Patient
          Report: Report
          Order: Order
          TumorSample: Sample<TumorCategory>
          NormalSample: Sample<NormalCategory> option
        //   Results: Results
        }

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
            match hgvs.``Protein Sequence Change`` with
            | Some proteinSequenceChange -> proteinSequenceChange.AbbreviatedChange.Value
            | _ -> hgvs.``Coding DNA Sequence Change``.Value

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
                Ok <| { ``Protein Sequence Change`` = None
                        ``Coding DNA Sequence Change`` = ``HGVS Coding DNA Sequence Change`` json.``HGVS.c``
                        ReferenceSequence = ReferenceSequence json.Transcript }
            /// Both HGVS protein sequence change and coding DNA sequence change are present
            | (NotBlank, NotBlank, NotBlank, NotBlank, NotBlank) when json.``HGVS.p`` = json.MutationEffect ->
                Ok <| { ``Protein Sequence Change`` = Some { AbbreviatedChange = ``Abbreviated Protein Sequence Change`` json.``HGVS.p``
                                                             FullChange = ``Full Protein Sequence Change`` json.``HGVS.pFull`` }
                        ``Coding DNA Sequence Change`` = ``HGVS Coding DNA Sequence Change`` json.``HGVS.c``
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
                      SignoutDate        = signoutDateDecoder |> get.Required.Raw }
                )

    module Sample =
        open System

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
              TumorPercentage: int option }

            static member Decoder : Decoder<InstitutionJson> =
                Decode.object (fun get ->
                    { BlockId         = "blockId"         |> flip get.Optional.Field Decode.string
                      TumorPercentage = "tumorPercentage" |> flip get.Optional.Field Decode.int }
                )

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


    module ``Somatic Potentially Actionable Mutation`` =
        module Variant =
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
                        }
                    )

        type Json =
            { GeneJson: Gene.Json
              VariantJsons: Variant.Json list }

            static member Decoder =
                Decode.object (fun get ->
                    { GeneJson = get.Required.Raw Gene.Json.Decoder
                      VariantJsons = "variants" |> flip get.Required.Field (Decode.list Variant.Json.Decoder) }
                )

    module ``Somatic Potentially Actionable Copy Number Variant`` =
        type Json =
            { Gene: Gene.Json
              VariantDescription: string
              VariantType: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { Gene = get.Required.Raw Gene.Json.Decoder
                      VariantDescription = "variantDescription" |> flip get.Required.Field Decode.string
                      VariantType = "variantType" |> flip get.Required.Field Decode.string }
                )


    module ``Somatic Biologically Relevant Variant`` =
        module Gene =
            /// Validate that a gene or a fusion gene is present and valid
            let validate geneJson fusionGeneJson =
              match Gene.Json.validate geneJson, Fusion.validate fusionGeneJson with
              | (Ok gene, Ok fusionGene) -> Error $"Both gene and fusion gene json are valid: ({gene}, {fusionGene})"
              | (Error geneError, Error fusionError) -> Error $"Both gene and fusion gene are invalid: ({geneError}, {fusionError})"
              | (Ok gene, _) -> Ok <| Gene gene
              | (_, Ok fusion) -> Ok <| Fusion fusion

        /// Represents a json object found in results.somaticBiologicallyRelevantVariants
        type Json =
            { GeneJson: Gene.Json
              FusionGene: Fusion.Json
              HgvsJson: HGVS.Json
              NucleotideAlteration: string
              AllelicFraction: string
              VariantType: string
              VariantDescription: string
              StructuralVariant: string }

            static member Decoder : Decoder<Json> =
                Decode.object (fun get ->
                    { GeneJson   = Gene.Json.Decoder   |> get.Required.Raw
                      FusionGene = Fusion.Json.Decoder |> get.Required.Raw
                      HgvsJson   = HGVS.Json.Decoder   |> get.Required.Raw
                      VariantType          = "variantType" |> flip get.Required.Field Decode.string
                      VariantDescription   = "variantDescription"   |> flip get.Required.Field Decode.string
                      StructuralVariant    = "structuralVariant"    |> flip get.Required.Field Decode.string
                      NucleotideAlteration = "nucleotideAlteration" |> flip get.Required.Field Decode.string
                      AllelicFraction      = "allelicFraction"      |> flip get.Required.Field Decode.string }
                )

        let validate json =
            json

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
            open Utilities

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
                let! tmb = (json.TumorMutationBurden |> Option.map TumorMutationBurden.ScoreInput, json.TumorMutationBurdenPercentile |> Option.map TumorMutationBurden.PercentileInput) ||> TumorMutationBurden.validateOptional
                and! msiStatus = json.MsiStatus |> Option.map MicrosatelliteInstabilityStatus.Input |> MicrosatelliteInstabilityStatus.validateOptional

                return { TumorMutationBurden = tmb
                         MicrosatelliteInstabilityStatus = msiStatus }
            }


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
                    Results = "results"   |> flip get.Required.Field Results.Json.Decoder }
              )

    module Json =
        type Error =
          { FileName: string
            Error: string }

        let deserialize =
            Decode.fromString Json.Decoder

        let deserializeWithError fileName =
            deserialize
            >> Result.mapError (fun errMsg -> { FileName = fileName; Error = errMsg })

    module Database =
        open Database

        let toPatientRow (json: Json) =
            let row = context.Public.Patients.Create()
            let patient = json.Patient

            // row.Mrn <- patient.MRN.Value

            row

