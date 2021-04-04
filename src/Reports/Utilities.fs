namespace OSTOR.ClinicalTrials.Reports

module Utilities =

    let flip f a b =
        f b a

    module DateTime =
        let tryParse(input: string) =
            match System.DateTime.TryParse(input) with
            | (true, dateTime) -> Some dateTime
            | _ -> None

    module Float =
        let tryParse(input: string) =
            match System.Double.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module Integer =
        let tryParse (input: string) =
            match System.Int32.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module Integer64 =
        let tryParse (input: string) =
            match System.Int64.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module UnsignedInteger =
        let tryParse(input: string) : uint option =
            match System.UInt32.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module UnsignedInteger64 =
        let tryParse(input: string) : uint64 option =
            match System.UInt64.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module Decoder =
        open System
        open Thoth.Json.Net

        module Optional =

            /// Deserialize json field into an optional datetime from a value that can be one of:
            ///
            /// - `datetime`
            /// - `null`
            /// - blank string (i.e. `""`)
            let dateTime: Decoder<DateTime option> =
                let decodeDateTimeFromString = Decode.map DateTime.tryParse Decode.string

                Decode.oneOf [ Decode.option Decode.datetime // field is null or datetime
                               decodeDateTimeFromString // attempt to decode and parse string into datetime
                              ]

            /// Deserialize json field into an optional float from a value that can be one of:
            ///
            /// - `float`
            /// - `null`
            /// - blank string (i.e. `""`)
            let float : Decoder<float option> =
                let strDecoder = Decode.map Float.tryParse Decode.string

                Decode.oneOf [ Decode.option Decode.float; strDecoder ]

            /// Deserialize json field into an optional integer from a value that can be one of:
            ///
            /// - `int`
            /// - `null`
            /// - blank string (i.e. `""`)
            let integer : Decoder<int option> =
                let strDecoder = Decode.map Integer.tryParse Decode.string

                Decode.oneOf [ Decode.option Decode.int; strDecoder ]

            /// Deserialize json field into an optional integer from a value that can be one of:
            ///
            /// - `string` -> `Some string`
            /// - `null`  -> `None`
            /// - blank string (i.e. `""`) -> `None`
            let string: Decoder<string option> =
                Decode.string
                |> Decode.map (fun str ->
                    match str with
                    | "" | null -> None
                    | _ -> Some str)

            /// Deserialize json field into an optional integer from a value that can be one of:
            ///
            /// - `uint`
            /// - `null`
            /// - blank string (i.e. `""`)
            let unsignedInteger : Decoder<uint option> =
                let strDecoder = Decode.map UnsignedInteger.tryParse Decode.string

                Decode.oneOf [ Decode.option Decode.uint32; strDecoder ]

    module Guid =
        let tryParse(input: string) =
            match System.Guid.TryParse(input) with
            | (true, guid) -> Some guid
            | _ -> None

    module List =
        /// Flatten list of lists into just a 1-d list
        ///
        ///    flatten [[1]; [2; 3; 4]; [5; 6]] = [1; 2; 3; 4; 5; 6]
        let flatten list =
            List.collect id list

    module Option =
        let toResult error option =
            match option with
            | Some a -> Ok a
            | _ -> Error error

    module Regex =
        open System.Text.RegularExpressions

        /// Remove a regex expression from a string
        let remove regex string =
            Regex.Replace(string, regex, "")

    module Result =
        /// Combine a list of results into a result of lists
        let combine (results: Result<'ok, 'err> list): Result<'ok list, 'err list> =
            let initial : Result<'ok list, 'err list> = Ok []

            results
            |> List.fold (fun agg res ->
                match res with
                | Ok ok -> Result.map (fun oks -> ok :: oks) agg
                | Error err -> Result.mapError (fun errs -> err :: errs) agg
            ) initial

        /// Partition a list of results into a tuple of successes and errors.
        ///
        ///    partition (Ok 1; Ok 2; Error "foo"; Ok 3; Error "bar") = ([1; 2; 3], ["foo"; "bar"])
        let partition (results: Result<'ok, 'err> list) =
            let initialPartition = ([], [])

            results
            |> List.fold (fun (oks, errors) result ->
                match result with
                | Ok ok -> (ok :: oks, errors)
                | Error error -> (oks, error :: errors)
            ) initialPartition

        let isOk result =
            match result with
            | Ok _ -> true
            | _ -> false

        let isError result =
            not (isOk result)

        let toOk result =
            match result with
            | Ok ok -> Some ok
            | _ -> None

        let toError result =
            match result with
            | Error err -> Some err
            | _ -> None

    module String =
        open System.Text.RegularExpressions
        open System.IO

        let contains (char: string) (str: string) =
            str.Contains(char)

        let isBlank str =
            str = ""

        let isNotBlank (str: string) =
            str <> ""

        let matches (regex: Regex) (str: string) =
            regex.Match(str).Success

        let split char (string: string) =
            string.Split([| char |]) |> Array.toList

        let readFromDisk path = File.ReadAllText(path)

        let writeToDisk path string =
            File.WriteAllText(path, string)

    module StringValidations =
        let (|BlankString|NotBlank|) str =
            if str <> "" then NotBlank
            else BlankString

        let validateNotBlank str =
            match str with
            | NotBlank -> Ok str
            | _ -> Error "Can't be blank"

        let validateDateTime str =
            str
            |> DateTime.tryParse
            |> Option.toResult $"Invalid DateTime: {str}"

        open System.Text.RegularExpressions

        let validateRegex regex str =
            if Regex(regex).Match(str).Success then
                Ok str
            else
                Error $"{regex} does not match {str}"

        module Typed =
            let validateNotBlank (createValidType: string -> 'validType) errorMessage input =
                input
                |> validateNotBlank
                |> Result.map createValidType
                |> Result.mapError (fun _ -> errorMessage)


    module FloatValidations =
        let ``validate >= 0`` (x: float) =
            if x >= 0.0 then Ok x
            else Error $"{x} is not >= 0"

    module Optional =
        /// Validate an optional input, if it exists. If an input does not exist, it is still valid.
        let validateWith validator (optionalInput: 'input option) =
            match optionalInput with
            | None -> Ok None
            | Some input ->
                match validator input with
                | Ok ok -> Ok <| Some ok
                | Error e -> Error e

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
