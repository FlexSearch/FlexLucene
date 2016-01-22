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
    directory.Close()

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

open Com.Spatial4j.Core.Context
let SimpleSpatialTests() = 
    let ctx = SpatialContext.GEO
    let grid = new GeohashPrefixTree(ctx, 11)
    let strategy = new RecursivePrefixTreeStrategy(grid, "myGeoField")
    let doc = new Document()
    doc.Add(new IntField("id", 1, FieldStore.YES))
    let pt = ctx.MakePoint(10.0, 10.0)
    doc.Add(new StoredField(strategy.GetFieldName(), pt.GetX().ToString() + " " + pt.GetY().ToString()))

let MutableValueDoesNotHaveDuplicateMethods() =
    let value = new FlexLucene.Util.Mutable.MutableValueBool()
    value._Exists <- true;
    Assert.True(value.Exists() = true, "Should be able to use the _ method in case of naming conflicts.")

let ToStringWorksCorrectly() =
    let query = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    printfn "RangeQuery ToString : %s" (query.ToString())
    Assert.Equal<string>(query.toString(), query.ToString())
    let query = new TermQuery(new Term("Hello"))
    printfn "TermQuery ToString : %s" (query.ToString())
    Assert.Equal<string>(query.toString(), query.ToString())

open Com.Spatial4j.Core.Shape.Impl
open Com.Spatial4j.Core.Shape
let JavaToStringOverrideIsPickedUpByGeneratedToString() =
    let baseClass = new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO)
    printfn "RectangleImpl ToString : %s" <| baseClass.ToString()
    Assert.Equal<string>(baseClass.toString(), baseClass.ToString())

    // Override java's toString()
    let javaOverrideClass = { new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO) with
            override this.toString() = base.toString() + "<overridden>" 
            override this.GetBuffered(d : float, sc : SpatialContext) : Shape = base.GetBuffered(d, sc) :> Shape }
    printfn "override toString RectangleImpl ToString : %s" <| javaOverrideClass.ToString()
    Assert.Equal<string>(javaOverrideClass.toString(), javaOverrideClass.ToString())
    Assert.Equal<string>(javaOverrideClass.ToString(), baseClass.toString() + "<overridden>")

    // Overriding FlexLucene's generated ToString() will just override System.Object's virtual ToString().
    // However, when calling the derived class, the generated FlexLucene's ToString method will be called (see e.g. *1*).
    // System.Object's ToString() method from the derived class will be called only if you cast the derived
    // instance to object (see e.g. *2*).
    let flexOverrideClass = { new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO) with
            override this.ToString() = base.ToString() + "<overridden>" 
            override this.GetBuffered(d : float, sc : SpatialContext) : Shape = base.GetBuffered(d, sc) :> Shape }
    printfn "RectangleImpl ToString without casting: %s" <| flexOverrideClass.ToString()
    printfn "RectangleImpl ToString with casting: %s" <| (flexOverrideClass :> obj).ToString()
    // E.g. *1*
    Assert.Equal<string>(baseClass.toString(), flexOverrideClass.ToString())
    // E.g. *2*
    Assert.Equal<string>(baseClass.toString() + "<overridden>", (flexOverrideClass :> obj).ToString())
    Assert.Equal<string>(flexOverrideClass.toString(), baseClass.toString())


let EqualityWorksCorrectly() =
    let query1 = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    let query2 = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    let query3 = NumericRangeQuery.NewDoubleRange("test3", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    Assert.Equal<Query>(query1, query2)
    Assert.NotEqual<Query>(query1, query3)
    let termQuery1 = new TermQuery(new Term("Hello"))
    let termQuery2 = new TermQuery(new Term("Hello"))
    Assert.Equal<int>(termQuery1.GetHashCode(), termQuery1.hashCode())
    Assert.Equal<Query>(termQuery1, termQuery2)

type DummyIndexInput(dummy : string) =
    inherit IndexInput(dummy)
        override __.Length() = 0L
        override __.Seek(l) = ()
        override __.close() = ()
        override __.GetFilePointer() = 0L
        override this.Slice(a, b, c) = this :> IndexInput
        override __.ReadBytes(a, b, c) = ()
        override __.ReadByte() = 1uy
        override this.equals(a) = a.GetHashCode() = this.GetHashCode() 
        override __.hashCode() = dummy.Length

let GetHashCodeCallshashCodeBehindTheScene() =
    let a = new DummyIndexInput("test1")
    let b = new DummyIndexInput("test02")
    let c = new DummyIndexInput("test1")
    printfn "Hash Code tests"
    printfn "a GetHashCode: %i" <| a.GetHashCode()
    printfn "a hashCode: %i" <| a.hashCode()
    printfn "b GetHashCode: %i" <| b.GetHashCode()
    printfn "b hashCode: %i" <| b.hashCode()
    printfn "c GetHashCode: %i" <| c.GetHashCode()
    printfn "c hashCode: %i" <| c.hashCode()
    Assert.True(a.GetHashCode() = a.hashCode())
    Assert.True(b.GetHashCode() = b.hashCode())
    Assert.True(c.GetHashCode() = c.hashCode())
    Assert.False(a.GetHashCode() = b.GetHashCode())
    Assert.False(a.hashCode() = b.hashCode())
    Assert.True(a.GetHashCode() = c.GetHashCode())
    Assert.True(a.hashCode() = c.hashCode())
    Assert.True(a.Equals(c))
    Assert.True(a.equals(c))
    Assert.True(c.Equals(a))
    Assert.True(c.equals(a))
    Assert.False(a.Equals(b))
    Assert.False(a.equals(b))
    
let GeneratedEqualsCallsJavaEqualsBehindTheScenes() =
    let baseClass = new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO)
    let baseClass2 = new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO)

    Assert.Equal(baseClass, baseClass)
    Assert.True(baseClass.Equals(baseClass))
    Assert.True((baseClass = baseClass))
    Assert.Equal(baseClass, baseClass2)

    let javaOverrideClass = { new RectangleImpl(1.0, 1.0, 1.0, 1.0, SpatialContext.GEO) with
            override this.equals(obj) = false 
            override this.GetBuffered(d : float, sc : SpatialContext) : Shape = base.GetBuffered(d, sc) :> Shape }

    Assert.False(javaOverrideClass.Equals(javaOverrideClass))
    Assert.False((javaOverrideClass = javaOverrideClass))

let executeTests() = 
    [| 
        CodecsShouldLoadProperly
        PostingsFormatShouldLoadProperly 
        IndexingTests
        BooleanQueryCreationTests 
        RangeQueryCreationTests 
        SimpleAnalyzerInitTest
        TokenizationTest
        SimpleSpatialTests
        MutableValueDoesNotHaveDuplicateMethods
        ToStringWorksCorrectly
        JavaToStringOverrideIsPickedUpByGeneratedToString
        EqualityWorksCorrectly
        GetHashCodeCallshashCodeBehindTheScene
        GeneratedEqualsCallsJavaEqualsBehindTheScenes
    |] 
    |> Array.iter exceptionWrapper
    if hasErrors then
        printfn "Some Tests failed"
        1
    else 
        printfn "All tests passed"
        0

executeTests()