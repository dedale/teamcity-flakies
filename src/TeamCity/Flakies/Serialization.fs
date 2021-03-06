module TeamCity.Flakies.Serialization

open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization

let rec private getFieldsRecursively (t: Type) =
    [ if t.BaseType <> null then
          for f in getFieldsRecursively t.BaseType do
              yield f
      let flags =
          BindingFlags.Public
          ||| BindingFlags.NonPublic
          ||| BindingFlags.Instance

      for f in t.GetFields(flags) do
          yield f ]

type FieldContractResolver private () =
    inherit DefaultContractResolver()

    // Needed to call overriden method in lambda below
    member private __.CreateProperty(f) =
        base.CreateProperty(f, MemberSerialization.Fields)

    override this.CreateProperties(t, _) =
        getFieldsRecursively t
        |> List.map (fun f -> this.CreateProperty(f))
        |> (fun list -> ResizeArray<JsonProperty>(list) :> IList<JsonProperty>)

    override __.CreateObjectContract(t) =
        let contract = base.CreateObjectContract(t)
        contract.DefaultCreator <- fun () -> FormatterServices.GetUninitializedObject(contract.UnderlyingType)
        contract

    static member Instance = FieldContractResolver()

[<AbstractClass; Sealed>]
type TextSerializer private () =
    static member private getSettings(?formatting) =
        let settings = JsonSerializerSettings()
        settings.TypeNameHandling <- TypeNameHandling.All
        settings.ObjectCreationHandling <- ObjectCreationHandling.Auto
        settings.ContractResolver <- FieldContractResolver.Instance
        settings.Formatting <- defaultArg formatting Formatting.None
        settings

    static member serialize(instance: 'a, ?formatting) =
        if typedefof<'a> = typedefof<string> then
            instance.ToString()
        else
            let formatting = defaultArg formatting Formatting.None
            let settings = TextSerializer.getSettings (formatting)
            JsonConvert.SerializeObject(instance, settings)

    static member deserialize<'a>(str, ?formatting) : 'a =
        if typedefof<'a> = typedefof<string> then
            box str :?> 'a
        else
            let formatting = defaultArg formatting Formatting.None
            let settings = TextSerializer.getSettings (formatting)
            let instance = JsonConvert.DeserializeObject<'a>(str, settings)

            if box instance = null then
                raise (JsonSerializationException "instance is null")
            else
                instance
