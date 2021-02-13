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
        let tryParse(input: string) =
            match System.Int32.TryParse(input) with
            | (true, num) -> Some num
            | _ -> None

    module Decoder =
        open Thoth.Json.Net

        /// Deserialize json field into an optional float from a value that can be one of:
        ///
        /// - `float`
        /// - `null`
        /// - blank string (i.e. `""`)
        let optionalFloat : Decoder<float option> =
            let strDecoder = Decode.map Float.tryParse Decode.string

            Decode.oneOf [ Decode.option Decode.float; strDecoder ]

        /// Deserialize json field into an optional integer from a value that can be one of:
        ///
        /// - `int`
        /// - `null`
        /// - blank string (i.e. `""`)
        let optionalInteger : Decoder<int option> =
            let strDecoder = Decode.map Integer.tryParse Decode.string

            Decode.oneOf [ Decode.option Decode.int; strDecoder ]

        /// Deserialize json field into an optional integer from a value that can be one of:
        ///
        /// - `string` -> `Some string`
        /// - `null`  -> `None`
        /// - blank string (i.e. `""`) -> `None`
        let optionalString : Decoder<string option> =
            let blankStringDecoder = Decode.map (fun str -> if str = "" then None else Some str) Decode.string

            Decode.oneOf [Decode.option Decode.string; blankStringDecoder]

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
