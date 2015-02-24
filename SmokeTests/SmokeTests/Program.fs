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

let mutable hasErrors = false

let exceptionWrapper (meth : unit -> unit) = 
    try 
        meth()
    with e -> 
        hasErrors <- true
        printfn "%A" e

let ShouldHaveFlexCodec50() = Assert.True(Codec.AvailableCodecs().contains("FlexCodec50"), "FlexCodec50 not found.")
let ShouldHaveFlexCodec410() = Assert.True(Codec.AvailableCodecs().contains("FlexCodec410"), "FlexCodec410 not found.")
let ShouldHaveFlexPerFieldPostingsFormat() = 
    Assert.True
        (PostingsFormat.AvailablePostingsFormats().contains("PerField40"), "PerField40 Postings format not found.")

let IndexingTest(directory : FlexLucene.Store.Directory) = 
    let analyzer = new StandardAnalyzer()
    let config = new IndexWriterConfig(analyzer)
    config.SetInfoStream(new FlexLucene.Util.PrintStreamInfoStream(new java.io.PrintStream(@"C:\git\FlexLucene.debug"))) |> ignore
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

[<Fact>]
let CodecsShouldLoadProperly() = 
    ShouldHaveFlexCodec410()
    ShouldHaveFlexCodec50()

[<Fact>]
let PostingsFormatShouldLoadProperly() = ShouldHaveFlexPerFieldPostingsFormat()

[<Fact>]
let SimpleIndexingTest() = 
    let analyzer = new StandardAnalyzer()
    let directory = new RAMDirectory()
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
    directory.Close()

[<Fact>]
let SimplePhysicalIndexingTest() = 
    IndexingTest(new RAMDirectory())
    let dir = Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, Guid.NewGuid().ToString())
    let file = new java.io.File(dir) 
    IndexingTest(new SimpleFSDirectory(file.toPath(), NativeFSLockFactory.INSTANCE))

[<Fact>]
let SimpleFlexPhysicalIndexingTest() = 
    IndexingTest(new RAMDirectory())
    let directoryPath = Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, Guid.NewGuid().ToString("N"))
    let file = new java.io.File(directoryPath)
    IndexingTest(FlexFSDirectory.Open(file.toPath()))

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
    let pt = ctx.MakePoint (10.0, 10.0)
    doc.Add(new StoredField(strategy.GetFieldName(), pt.getX().ToString() + " " + pt.getY().ToString()))


[<EntryPoint>]
let main argv = 
    [| CodecsShouldLoadProperly; SimpleIndexingTest; BooleanQueryCreationTests; SimplePhysicalIndexingTest; 
       SimpleFlexPhysicalIndexingTest; RangeQueryCreationTests; SimpleSpatialTests |] 
    |> Array.iter (fun meth -> exceptionWrapper meth)
    printfn "Done"
    if hasErrors then 1
    else 0