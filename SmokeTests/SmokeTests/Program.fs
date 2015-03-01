open Com.Spatial4j.Core.Context
open Com.Spatial4j.Core.Shape
open FlexLucene.Analysis.Standard
open FlexLucene.Codecs
open FlexLucene.Document
open FlexLucene.Index
open FlexLucene.Queryparser.Classic
open FlexLucene.Search
open FlexLucene.Spatial.Prefix
open FlexLucene.Spatial.Prefix.Tree
open FlexLucene.Store
open Xunit
open System
open System.IO
open Autofac
open Autofac.Extras.Attributed
open FlexLucene.Analysis
open Autofac.Features.Metadata
open System.Collections.Generic

let mutable hasErrors = false

let exceptionWrapper (meth : unit -> unit) = 
    try 
        meth()
        ()
    with e -> 
        hasErrors <- true
        printfn "------------------------------------------------"
        printfn "Test name: %s" (meth.GetType().FullName)
        printfn "%A" e
        printfn "------------------------------------------------"

let ShouldHaveFlexCodec50() = Assert.True(Codec.AvailableCodecs().contains("FlexCodec50"), "FlexCodec50 not found.")
let ShouldHaveFlexCodec410() = Assert.True(Codec.AvailableCodecs().contains("FlexCodec410"), "FlexCodec410 not found.")
let ShouldHaveFlexPerFieldPostingsFormat() = 
    Assert.True
        (PostingsFormat.AvailablePostingsFormats().contains("PerField40"), "PerField40 Postings format not found.")

let IndexingTest(directory : FlexLucene.Store.Directory) = 
    let analyzer = new StandardAnalyzer()
    let config = new IndexWriterConfig(analyzer)
    let iwriter = new IndexWriter(directory, config)
    let doc = new Document()
    let text = "This is the text to be indexed."
    doc.Add(new Field("fieldname", text, TextField.TYPE_STORED))
    iwriter.AddDocument(doc)
    iwriter.Close()
    // Now search the index:
    let ireader = DirectoryReader.Open(directory)
    let isearcher = new IndexSearcher(ireader)
    // Parse a simple query that searches for "text":
    let parser = new QueryParser("fieldname", analyzer)
    let query = parser.Parse("text")
    let topDocs = isearcher.Search(query, null, 1000)
    let hits : ScoreDoc [] = topDocs.ScoreDocs
    Assert.Equal<int>(1, hits.Length)
    // Iterate through the results:
    for i = 0 to hits.Length - 1 do
        let hitDoc = isearcher.Doc(hits.[i].Doc)
        Assert.Equal<string>("This is the text to be indexed.", hitDoc.Get("fieldname"))
    ireader.Close()
    directory.close()

let GetRandomPath() = 
    let dir = Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, Guid.NewGuid().ToString())
    (new java.io.File(dir)).toPath()

let RegisterAbstractClassAssemblies<'T>(builder : ContainerBuilder) = 
    // Returns true if the given type extends any of the given class names
    let rec extendsAnyOf (classNames : string seq) (_type: Type) =
        try
            match _type.BaseType with
            | null -> false
            | t when classNames |> Seq.exists (fun c -> c.ToLower() = t.FullName.ToLower()) -> true
            | t when t.FullName.ToLower() = "java.lang.object" -> false
            | _ -> _type.BaseType |> extendsAnyOf classNames
        with
            | ex -> false
    builder
        .RegisterAssemblyTypes(AppDomain.CurrentDomain.GetAssemblies())
        .Where(fun t -> t |> extendsAnyOf [typeof<'T>.FullName])
        .As<'T>() 
    |> ignore

[<Fact>]
let CodecsShouldLoadProperly() = 
    ShouldHaveFlexCodec410 |> exceptionWrapper
    ShouldHaveFlexCodec50 |> exceptionWrapper

[<Fact>]
let PostingsFormatShouldLoadProperly() = ShouldHaveFlexPerFieldPostingsFormat()

[<Fact>]
let IndexingTests() = 
    (fun _ -> IndexingTest(new RAMDirectory())) |> exceptionWrapper
    (fun _ -> IndexingTest(new SimpleFSDirectory(GetRandomPath(), NativeFSLockFactory.INSTANCE))) |> exceptionWrapper
    (fun _ -> IndexingTest(new MMapDirectory(GetRandomPath(), NativeFSLockFactory.GetDefault()))) |> exceptionWrapper
    (fun _ -> IndexingTest(FSDirectory.Open(GetRandomPath()))) |> exceptionWrapper

[<Fact>]
let BooleanQueryCreationTests() = 
    let query = new BooleanQuery(true)
    query.Add(new BooleanClause(new TermQuery(new Term("dummy")), BooleanClause.Occur.MUST))

[<Fact>]
let RangeQueryCreationTests() = 
    let query = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    ()

[<Fact>]
let SimpleSpatialTests() = 
    let ctx = SpatialContext.GEO
    let grid = new GeohashPrefixTree(ctx, 11)
    let strategy = new RecursivePrefixTreeStrategy(grid, "myGeoField")
    let doc = new Document()
    doc.Add(new IntField("id", 1, Field.Store.YES))
    let pt = ctx.MakePoint(10.0, 10.0)
    doc.Add(new StoredField(strategy.GetFieldName(), pt.getX().ToString() + " " + pt.getY().ToString()))

[<Fact>]
let FlexLuceneAttributeIsAttachedToAnalyzer() =
    let hasAtt = 
        typedefof<FlexLucene.Analysis.Br.BrazilianAnalyzer>.CustomAttributes
        |> Seq.exists (fun a -> a.AttributeType = typedefof<FlexSearch.Core.Attributes.AutofacNameAttribute>)
    Assert.True(hasAtt)

[<Fact>]
let AutofacCanIdentifyTaggedLuceneClasses() =
    let builder = new ContainerBuilder()
    builder.RegisterModule<AttributedMetadataModule>() |> ignore
    builder |> RegisterAbstractClassAssemblies<FlexLucene.Analysis.Analyzer>
    let container = builder.Build()

    let scope = container.BeginLifetimeScope()
    let classes = scope.Resolve<IEnumerable<Meta<Lazy<Analyzer>>>>()
    let hasAnalyzer = classes |> Seq.exists (fun c -> 
        c.Metadata.Keys.Contains("Name")
        && String.Equals(c.Metadata.["Name"].ToString(), "BrazilianAnalyzer", StringComparison.OrdinalIgnoreCase))
    Assert.True(hasAnalyzer)


[<EntryPoint>]
let main argv = 
    [| CodecsShouldLoadProperly; IndexingTests; BooleanQueryCreationTests; RangeQueryCreationTests; SimpleSpatialTests;
       FlexLuceneAttributeIsAttachedToAnalyzer; AutofacCanIdentifyTaggedLuceneClasses |] 
    |> Array.iter (fun meth -> exceptionWrapper meth)
    printfn "Done"
    if hasErrors then 1
    else 0
