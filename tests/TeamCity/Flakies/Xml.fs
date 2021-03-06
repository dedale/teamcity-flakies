module TeamCity.Flakies.Tests.Xml

open System
open System.Xml.Linq

[<NoComparison>]
type XmlContainer =
    | XmlElement of string * XmlContainer list
    | Attribute of string * obj
    | XmlValue of string

    static member Element(name: string, [<ParamArray>] children: XmlContainer []) : XmlContainer =
        XmlElement(name, children |> List.ofArray)

    static member Element(name: string, value: string) : XmlContainer = XmlElement(name, [ XmlValue value ])

    override x.ToString() =
        let rec fromContainer container : obj =
            match container with
            | XmlElement (name, children) -> XElement(XName.op_Implicit name, children |> List.map fromContainer) :> obj
            | Attribute (name, value) -> XAttribute(XName.op_Implicit name, value) :> obj
            | XmlValue value -> value :> obj

        match x with
        | XmlElement (name, children) ->
            XElement(XName.op_Implicit name, children |> List.map fromContainer |> Array.ofList).ToString()
        | Attribute (name, value) -> XAttribute(XName.op_Implicit name, value).ToString()
        | XmlValue value -> value

let outerXml (x: XmlContainer) = x.ToString()
