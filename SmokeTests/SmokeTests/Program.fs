// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Xunit
open FlexLucene.Analysis.Standard
open FlexLucene.Index
open FlexLucene.Document
open FlexLucene.Queryparser.Classic
open FlexLucene.Store
open FlexLucene.Search
open FlexLucene.Codecs

let mutable hasErrors = false

let exceptionWrapper (meth : unit -> unit) = 
    try 
        meth()
    with e -> 
        hasErrors <- true
        printfn "%A" e

[<Fact>]
let CodecsShouldLoadProperly() = 
    let codecs = Codec.AvailableCodecs()
    Assert.True(codecs.size() > 0)

[<Fact>]
let PostingsFormatShouldLoadProperly() = 
    let formats = PostingsFormat.AvailablePostingsFormats()
    Assert.True(formats.size() > 0)

[<Fact>]
let SimpleIndexingTest() = 
    let analyzer = new StandardAnalyzer()
    let directory = new RAMDirectory()
    let config = new IndexWriterConfig(analyzer)
    let iwriter = new IndexWriter(directory, config)
    let doc = new Document()
    let text = "This is the text to be indexed."
    doc.add(new Field("fieldname", text, TextField.TYPE_STORED))
    iwriter.AddDocument(doc)
    iwriter.close()
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
    ireader.close()
    directory.close()

[<Fact>]
let BooleanQueryCreationTests() = 
    let query = new BooleanQuery(true)
    
    query.add (new BooleanClause(new TermQuery(new Term("dummy")), BooleanClause.Occur.MUST))

[<Fact>]
let RangeQueryCreationTests() = 
    let query = NumericRangeQuery.NewDoubleRange("test", java.lang.Double(32.0), java.lang.Double(33.0), true, true)
    ()

[<EntryPoint>]
let main argv = 
    [| CodecsShouldLoadProperly; PostingsFormatShouldLoadProperly; SimpleIndexingTest; BooleanQueryCreationTests; 
       RangeQueryCreationTests |] |> Array.iter (fun meth -> exceptionWrapper meth)
    printfn "Done"
    if hasErrors then 1
    else 0
