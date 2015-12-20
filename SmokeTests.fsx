#r "Artifacts/FlexLucene.dll"
#r "ikvm/IKVM.OpenJDK.Core.dll"
#r "ikvm/IKVM.OpenJDK.text.dll"
#r "ikvm/IKVM.Runtime.dll"
#r "dll/xunit.dll"

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
open System.Collections.Generic

let mutable hasErrors = false
let RootDirectory = __SOURCE_DIRECTORY__
let TempDirectory = Path.Combine(RootDirectory, "work")
let exceptionWrapper (meth : unit -> unit) = 
    try 
        meth()
        ()
    with e -> 
        hasErrors <- true
        printfn "------------------------------------------------"
        printfn "Test case failed: %s" (meth.GetType().FullName)
        printfn "%A" e
        printfn "------------------------------------------------"
        
let ShouldHaveCodec50() = Assert.True(Codec.AvailableCodecs().contains("Lucene50"), sprintf "Lucene50Codec not found. Available Codecs: %A" (Codec.AvailableCodecs()))
let ShouldHaveCodec410() = Assert.True(Codec.AvailableCodecs().contains("Lucene410"), sprintf "Lucene410Codec not found. Available Codecs: %A" (Codec.AvailableCodecs()))

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
    let dir = Path.Combine(TempDirectory, Guid.NewGuid().ToString())
    (new java.io.File(dir)).toPath()

let CodecsShouldLoadProperly() = 
    let codecs = Codec.AvailableCodecs()
    printfn "Available Codecs: %A" (codecs)
    ShouldHaveCodec410 |> exceptionWrapper
    ShouldHaveCodec50 |> exceptionWrapper

let PostingsFormatShouldLoadProperly() = 
    printfn "Postings Format : %A" (PostingsFormat.AvailablePostingsFormats())
    Assert.True
        (PostingsFormat.AvailablePostingsFormats().contains("Lucene50"), "Lucene50 Postings format not found.")

let IndexingTests() = 
    (fun _ -> IndexingTest(new RAMDirectory())) |> exceptionWrapper
    (fun _ -> IndexingTest(new SimpleFSDirectory(GetRandomPath(), NativeFSLockFactory.INSTANCE))) |> exceptionWrapper
    (fun _ -> IndexingTest(new MMapDirectory(GetRandomPath(), NativeFSLockFactory.GetDefault()))) |> exceptionWrapper
    (fun _ -> IndexingTest(FSDirectory.Open(GetRandomPath()))) |> exceptionWrapper

let BooleanQueryCreationTests() = 
    let query = new BooleanQuery(true)
    query.Add(new BooleanClause(new TermQuery(new Term("dummy")), BooleanClauseOccur.MUST))

let RangeQueryCreationTests() = 
    let query = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    ()

open FlexLucene.Analysis
open FlexLucene.Analysis.Core
// Simple analyzer can be written and the base class methods are
// PascalCased as per C# convention
type SimpleAnalyzer() = 
    inherit Analyzer()
    override __.CreateComponents (_) = 
        let src = new StandardTokenizer()
        let filter = new LowerCaseFilter(src)
        new AnalyzerTokenStreamComponents(src, filter)

let SimpleAnalyzerInitTest() =
    let analyzer = new SimpleAnalyzer()
    ()

let flexCharTermAttribute = 
        lazy java.lang.Class.forName 
                 (typeof<FlexLucene.Analysis.Tokenattributes.CharTermAttribute>.AssemblyQualifiedName)

/// Utility function to get tokens from the search string based upon the passed analyzer
/// This will enable us to avoid using the Lucene query parser
/// We cannot use simple white space based token generation as it really depends 
/// upon the analyzer used
let ParseTextUsingAnalyzer (analyzer : FlexLucene.Analysis.Analyzer, fieldName, queryText) = 
    let tokens = new List<string>()
    let source : TokenStream = analyzer.TokenStream(fieldName, new java.io.StringReader(queryText))
    // Get the CharTermAttribute from the TokenStream
    let termAtt = source.AddAttribute(flexCharTermAttribute.Value)
    try 
        try 
            source.Reset()
            while source.IncrementToken() do
                tokens.Add(termAtt.ToString())
            source.End()
        with _ -> ()
    finally
        source.Close()
    tokens

// An advance test which tests usage of CharTermAttribute
let TokenizationTest() =
    let analyzer = new SimpleAnalyzer()
    let result = ParseTextUsingAnalyzer (analyzer, "", "Hello World")
    let expected = new List<string>([|"hello"; "world"|])
    printfn "Tokenization Test"
    printfn "Expected : %A" expected
    printfn "Actual : %A" result
    Assert.Equal<List<string>>(expected, result)

//[<Fact>]
//let SimpleSpatialTests() = 
//    let ctx = SpatialContext.GEO
//    let grid = new GeohashPrefixTree(ctx, 11)
//    let strategy = new RecursivePrefixTreeStrategy(grid, "myGeoField")
//    let doc = new Document()
//    doc.Add(new IntField("id", 1, Field.Store.YES))
//    let pt = ctx.MakePoint(10.0, 10.0)
//    doc.Add(new StoredField(strategy.GetFieldName(), pt.getX().ToString() + " " + pt.getY().ToString()))

let executeTests() = 
    [| 
        CodecsShouldLoadProperly
        PostingsFormatShouldLoadProperly 
        IndexingTests
        BooleanQueryCreationTests 
        RangeQueryCreationTests 
        SimpleAnalyzerInitTest
        TokenizationTest
    |] 
    |> Array.iter exceptionWrapper
    if hasErrors then
        printfn "Some Tests failed"
        1
    else 
        printfn "All tests passed"
        0

executeTests()