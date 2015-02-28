namespace FlexSearch.Core.Attributes

open System.ComponentModel.Composition
open System

/// <summary>
/// Represents the lookup name for the plug-in
/// </summary>
[<MetadataAttribute>]
[<Sealed>]
type AutofacNameAttribute(name : string) = 
    inherit Attribute()
    member this.Name = name
