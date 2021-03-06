namespace TeamCity.Flakies

open System.Xml.Linq

[<AutoOpen>]
module XLinqExtensions =
    type XElement with
        member x.AttributeValue name =
            let attribute = x.Attribute(XName.op_Implicit name)

            let value =
                if attribute <> null then
                    attribute.Value
                else
                    null

            match value with
            | null -> None
            | x -> Some x

        member x.ElementContent name =
            match x.Element(XName.op_Implicit name) with
            | null -> None
            | e -> Some e
