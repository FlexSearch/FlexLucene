#r "System.Linq"
#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"
#r "dll/Mono.Cecil.dll"
#r "dll/Mono.Cecil.Mdb.dll"
#r "dll/Mono.Cecil.Pdb.dll"
#r "dll/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Cecil.Cil
open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Linq

// ---------------------------------------------
// Generic Helpers
// ---------------------------------------------
let (<!!>) (path1 : string) (path2 : string) = Path.Combine([| path1; path2 |])
let loopDir (dir : string) = Directory.EnumerateDirectories(dir)
let loopFiles (dir : string) = Directory.EnumerateFiles(dir)
let createDir (dir : string) = Directory.CreateDirectory(dir) |> ignore
let (!>) (content : string) = printfn "[INFO] %s" content |> ignore
let (!>>) (content : string) = printfn "\t%s" content |> ignore
let brk() = !>"------------------------------------------------------------------------"
// ---------------------------------------------
// Build script configuration section
// Modify these to update the versioning information
// ---------------------------------------------
let IkvmVersion = "8.1.5717.0"
let LuceneVersion = "5.3.1"
let LuceneFullVersion = "5.3.1.1"
let FileVersion = "5.3.1.1"

brk()
!>"Starting FlexLucene Build"
brk()
!>"Global Variable Information"
!>>(sprintf "IKVM Version: %s" IkvmVersion)
!>>(sprintf "Lucene Version: %s" LuceneVersion)
!>>(sprintf "Lucene full Version: %s" LuceneFullVersion)
!>>(sprintf "File Version: %s" FileVersion)

let JAVA_HOME = Environment.GetEnvironmentVariable("JAVA_HOME")

!>>(sprintf "JAVA_HOME: %s" JAVA_HOME)
brk()

[<AutoOpenAttribute>]
module Helpers = 
    /// <summary>
    /// Converts Java namespaces, method names to .net based conventions 
    /// </summary>
    /// <param name="input"></param>
    let ConvertNamingConvention(input : string) = 
        let output = new ResizeArray<Char>()
        let mutable ip = input.Replace("org.apache.lucene", "FlexLucene")
        ip <- ip.Replace("$", "")
        for i = 0 to ip.Length - 1 do
            if i = 0 then output.Add(Char.ToUpper(ip.[i]))
            else if ip.[i - 1] = '.' then output.Add(Char.ToUpper(ip.[i]))
            else output.Add(ip.[i])
        new System.String(output.ToArray())
    
    // Create the content for new SPI files
    let CreateServiceFile(filePath : string) = 
        Seq.fold (fun (acc : string list) (elem : string) -> 
            if elem.StartsWith("org.apache") || elem.StartsWith("Flex") then ConvertNamingConvention elem :: acc
            else acc) [] (File.ReadAllLines(filePath))
    
    /// <summary>
    /// Empty a given directory
    /// </summary>
    /// <param name="folderPath"></param>
    let CreateAndEmptyDirectory(folderPath : string) = 
        createDir folderPath
        loopDir folderPath |> Seq.iter (fun x -> Directory.Delete(x, true))
        loopFiles folderPath |> Seq.iter (fun x -> File.Delete(x))
        folderPath
    
    /// <summary>
    /// Executes a given exe alongwith the passed argument 
    /// </summary>
    /// <param name="path"></param>
    /// <param name="argument"></param>
    let Exec(path, argument) = 
        let psi = new ProcessStartInfo()
        psi.FileName <- path
        psi.Arguments <- argument
        psi.WorkingDirectory <- __SOURCE_DIRECTORY__
        psi.RedirectStandardOutput <- false
        psi.UseShellExecute <- false
        use p = Process.Start(psi)
        p.WaitForExit()

// ---------------------------------------------
// Calculated Variables
// ---------------------------------------------
let RootDirectory = __SOURCE_DIRECTORY__
let WorkDirectory = RootDirectory <!!> "Work" |> CreateAndEmptyDirectory
let DllDirectory = RootDirectory <!!> "dll"
let IkvmPath = sprintf @"%s\ikvm\ikvmc.exe" RootDirectory
let VersionPatchPath = RootDirectory <!!> "verpatch.exe"
let LibSrcDirectory = RootDirectory <!!> "Lib"
let LibTargetDirectory = WorkDirectory <!!> "Lib" |> CreateAndEmptyDirectory
let FlexSearchJar = RootDirectory <!!> "FlexSearch.Codecs"
let TempDirectory = WorkDirectory <!!> "Temp" |> CreateAndEmptyDirectory
let MetaDirectory = WorkDirectory <!!> "Meta" |> CreateAndEmptyDirectory
let ServicesDirectory = MetaDirectory <!!> @"META-INF\services" |> CreateAndEmptyDirectory
let LuceneDirectory = WorkDirectory <!!> "Lucene" |> CreateAndEmptyDirectory
let OutputDirectory = WorkDirectory <!!> "Output" |> CreateAndEmptyDirectory
let SmokeTestArtifacts = RootDirectory <!!> @"SmokeTests\SmokeTestArtifacts"

!>>(sprintf "Root Directory: %s" RootDirectory)
!>>(sprintf "Work Directory: %s" WorkDirectory)

// List of all the lucene jars which will be combined to form FlexLucene
let LuceneJars = 
    [| sprintf "lucene-core-%s" LuceneVersion
       sprintf "lucene-analyzers-common-%s" LuceneVersion
       sprintf "lucene-analyzers-phonetic-%s" LuceneVersion
       sprintf "lucene-backward-codecs-%s" LuceneVersion
       sprintf "lucene-codecs-%s" LuceneVersion
       sprintf "lucene-queries-%s" LuceneVersion
       sprintf "lucene-queryparser-%s" LuceneVersion
       sprintf "lucene-facet-%s" LuceneVersion
       sprintf "lucene-suggest-%s" LuceneVersion
       sprintf "lucene-misc-%s" LuceneVersion
       sprintf "lucene-memory-%s" LuceneVersion
       sprintf "lucene-highlighter-%s" LuceneVersion
       sprintf "lucene-join-%s" LuceneVersion
       //sprintf "lucene-replicator-%s" LuceneVersion
       //sprintf "lucene-expressions-%s" LuceneVersion
       sprintf "lucene-grouping-%s" LuceneVersion
       sprintf "lucene-spatial-%s" LuceneVersion
       sprintf "lucene-spatial3d-%s" LuceneVersion
       sprintf "lucene-sandbox-%s" LuceneVersion |]

let JarFiles = Directory.GetFiles(RootDirectory, "*.jar", System.IO.SearchOption.AllDirectories)
let javaExec (args : string) = Exec(JAVA_HOME <!!> @"bin\java.exe", args)

/// <summary>
/// Compiles FlexSearch related custom Java code to a jar file using Maven
/// </summary>
let codecsCompilation() = 
    !>>"Compiling using MAVEN."
    Exec(FlexSearchJar <!!> "compile.bat", "")
    !>>"Copy FlexSearch.Codec-0.0.0.jar to Lucene Files directory"
    assert File.Exists(FlexSearchJar <!!> @"target\FlexSearch.Codec-0.0.0.jar")
    File.Copy(FlexSearchJar <!!> @"target\FlexSearch.Codec-0.0.0.jar", LuceneDirectory <!!> @"FlexSearch.Codec.jar")

/// <summary>
/// Copies all the required Lucene jar files to the target directory
/// </summary>
let copyLuceneFilesToTarget() = 
    LuceneJars |> Array.iter (fun jar -> 
                      match JarFiles.FirstOrDefault(fun x -> x.Contains(jar)) with
                      | null -> failwithf "Jar not found: %s" jar
                      | filePath -> 
                          !>>(sprintf "Copying: %s" filePath)
                          File.Copy(filePath, LuceneDirectory <!!> (Path.GetFileName(filePath))))

/// <summary>
/// Extracts meta data information from Lucene jars
/// </summary>
let extractMetaInformation() = 
    loopFiles LuceneDirectory |> Seq.iter (fun file -> 
                                     use archive = ZipFile.Open(file, ZipArchiveMode.Update)
                                     
                                     let toBeDeleted = 
                                         Seq.fold (fun (acc : string list) (entry : ZipArchiveEntry) -> 
                                             if entry.FullName.StartsWith(@"META-INF/services/") then 
                                                 let info = Directory.CreateDirectory(TempDirectory <!!> entry.Name)
                                                 entry.ExtractToFile(info.FullName <!!> (Guid.NewGuid().ToString()))
                                                 entry.FullName :: acc
                                             else if entry.FullName.StartsWith("META-INF") then entry.FullName :: acc
                                             else acc) [] archive.Entries
                                     toBeDeleted |> Seq.iter (fun res -> archive.GetEntry(res).Delete()))

/// <summary>
/// Builds FlexLucene compatible meta data information
/// </summary>
let generateMetaInformation() = 
    loopDir TempDirectory |> Seq.iter (fun dir -> 
                                 loopFiles dir |> Seq.iter (fun file -> 
                                                      let dirInfo = new DirectoryInfo(Path.GetDirectoryName(file))
                                                      let targetFileName = ConvertNamingConvention(dirInfo.Name)
                                                      let targetPath = ServicesDirectory <!!> targetFileName
                                                      File.AppendAllLines(targetPath, CreateServiceFile(file))))
    !>"Create meta data jar"
    ZipFile.CreateFromDirectory(MetaDirectory, LuceneDirectory <!!> "Metadata.jar")

/// <summary>
/// Copies related libraries which are needed for compilation
/// </summary>
let copyLibraries() = 
    loopFiles LibSrcDirectory |> Seq.iter (fun f -> File.Copy(f, LibTargetDirectory <!!> (Path.GetFileName(f))))

/// <summary>
/// Executes Proguard which create a fat FlexLucene jar file
/// </summary>
let executeProguard() = 
    !>>"Move proguard.cfg to work directory"
    File.Copy(RootDirectory <!!> "proguard.cfg", WorkDirectory <!!> "proguard.cfg")
    javaExec """-jar ProGuard.jar @work\proguard.cfg"""

/// Executed Proguard after mapping update
let executeProguardPass2() =
    !>> "Deleting the FlexLucene.jar created in Phase 1"
    File.Delete(WorkDirectory <!!> "FlexLucene.jar")
    !>> "Updating the proguard.cfg"
    let mutable text = File.ReadAllText(WorkDirectory <!!> "proguard.cfg")
    text <- text.Replace("#-applymapping mapping-in.txt", "-applymapping mapping-in.txt")
    text <- text.Replace("-printmapping mapping-out.txt", "#-printmapping mapping-out.txt")
    File.WriteAllText(WorkDirectory <!!> "proguard.cfg", text)
    !>> "Running Proguard Pass 2"
    javaExec """-jar ProGuard.jar @work\proguard.cfg"""

/// <summary>
///Generated IKVM build string
/// </summary>
let GetIkvmBuildString() = 
    let sb = new System.Text.StringBuilder()
    loopFiles LibTargetDirectory |> Seq.iter (fun f -> sb.Append(f).Append(" ") |> ignore)
    sb.Append(WorkDirectory <!!> "FlexLucene.jar") |> ignore
    sb.Append(sprintf " -target:library -out:%s/%s.dll -version:%s -fileversion:%s" OutputDirectory "FlexLucene" 
                  LuceneFullVersion FileVersion).ToString()

/// <summary>
/// Executes IKVM which build FlexLucene.dll
/// </summary>
let executeIkvm() = Exec(IkvmPath, GetIkvmBuildString())

/// <summary>
/// Adds build information to the generated Dll
/// </summary>
let addBuildInformation() = 
    let finalDllPath = OutputDirectory <!!> "FlexLucene.dll"
    let patchExec (args : string) = Exec(VersionPatchPath, args)
    patchExec (sprintf """%s %s /pv %s /va""" finalDllPath LuceneFullVersion FileVersion)
    patchExec (sprintf """%s /s FileDescription "Built using IKVM version %s" """ finalDllPath IkvmVersion)
    patchExec (sprintf """%s /s product "FlexSearch Search Engine" """ finalDllPath)
    patchExec (sprintf """%s /s copyright "(c) 2010-2015 FlexSearch" """ finalDllPath)

/// Generates the new mapping information for renaming classes in accordance 
/// with C# naming convention 
let generateRenameMapping() =
    let inputFile = WorkDirectory <!!> "mapping-out.txt"
    let lines = File.ReadAllLines(inputFile)
    let memberSpaces = "    "
    
    let split(input : string) = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    
    let processLine(line : string) =
        let parts = split line
        // Check if the line contains a class declaration
        if line.StartsWith(memberSpaces) then
            assert(parts.Length = 4)
            let name = 
                if parts.[1].Contains("(") then
                    // It is a method declaration
                    parts.[1].Substring(0, parts.[1].IndexOf("("))
                else parts.[1]
            if name = parts.[3] || name.Contains("<") then
                // The name of the member should not be changed
                line
            else
                sprintf "%s%s %s -> %s" memberSpaces parts.[0] parts.[1] (Helpers.ConvertNamingConvention(name))
        else
            assert(parts.Length = 3)
            // Get rid of the : in the end
            let name = parts.[2].Substring(0, parts.[2].Length - 1)
            // The before and after class names are same so
            // must be one of the classes which can't be renamed.
            if parts.[0] = name then
                line
            else
                sprintf "%s -> %s:" parts.[0] (Helpers.ConvertNamingConvention(parts.[0]))
        
    let output =
        lines |> Array.map processLine
    File.WriteAllLines(WorkDirectory <!!> "mapping-in.txt", output)

// ---------------------------------------------
// Mono Cecil based rewrite section
// ---------------------------------------------
module CecilWriter = 
    let mutable md : ModuleDefinition = Unchecked.defaultof<_>
    let mutable editorBrowsableCtor : MethodReference = Unchecked.defaultof<_>
    let mutable obsoleteCtor : MethodReference = Unchecked.defaultof<_>
    let mutable editorStateRef : TypeReference = Unchecked.defaultof<_>
    
    // Custom attribute to stop method from showing in intellisense
    let GetEditorBrowsableAttr() = 
        let attr = new CustomAttribute(editorBrowsableCtor)
        attr.Properties.Add
            (new CustomAttributeNamedArgument("EditorBrowsableState", new CustomAttributeArgument(editorStateRef, 1)))
        attr
    
    // Obsolete Attribute
    let GetObsAttr() = new CustomAttribute(obsoleteCtor)
    let mutable renameFileMethod : MethodBody = Unchecked.defaultof<_>
    
    let copyMethodBody (m : MethodDefinition) = 
        if m.FullName.Contains("FlexFSDirectory") && m.FullName.Contains("renameFile") then 
            renameFileMethod <- m.Body
            m.Body
        else 
            if m.FullName.Contains("FSDirectory") && m.FullName.Contains("renameFile") then m.Body <- renameFileMethod
            m.Body
    
    let ProcessMethods(typ : TypeDefinition) = 
        let newMethods = new ResizeArray<MethodDefinition>()
        for meth in typ.Methods do
            if meth.Name <> null && not meth.IsRuntimeSpecialName && not meth.IsSpecialName && not meth.IsConstructor 
               && not meth.IsNative && not meth.IsAssembly && not meth.IsPInvokeImpl && not meth.IsUnmanaged 
               && meth.IsPublic then 
                if meth.IsAbstract || meth.Name = ConvertNamingConvention(meth.Name) then 
                    meth.NoInlining <- false
                    meth.NoOptimization <- false
                else 
                    let newMeth = 
                        new MethodDefinition(ConvertNamingConvention(meth.Name), meth.Attributes, meth.ReturnType)
                    meth.Parameters |> Seq.iter (fun x -> newMeth.Parameters.Add(x))
                    // Remove the no in-line attribute from IKVM generated code as it is due to a
                    // .net 2.0 related issue
                    meth.NoInlining <- false
                    meth.NoOptimization <- false
                    newMeth.NoInlining <- false
                    newMeth.NoOptimization <- false
                    
                    newMeth.Body <- copyMethodBody meth
                    /// Copy all attributes from the existing method to the new method 
                    meth.CustomAttributes |> Seq.iter(fun x -> newMeth.CustomAttributes.Add(x))
                    newMethods.Add(newMeth)
                    meth.CustomAttributes.Add(GetEditorBrowsableAttr())
            else
                    meth.NoInlining <- false
                    meth.NoOptimization <- false
        newMethods |> Seq.iter (fun x -> typ.Methods.Add(x))
    
    let regenerateImplementsAtt (_type : TypeDefinition) = 
        if _type.HasCustomAttributes then 
            let implAtts = _type.CustomAttributes |> Seq.filter (fun a -> a.AttributeType.Name = "ImplementsAttribute")
            if implAtts
               |> Seq.length
               > 0 then 
                let implAtt = implAtts |> Seq.head
                let arg = implAtt.ConstructorArguments |> Seq.head
                if arg.Type.Name <> "String[]" then 
                    failwithf "Expected type of argument to be String[], but found %s" arg.Type.Name
                let javaTypes = arg.Value :?> CustomAttributeArgument []
                
                let newJavaTypes = 
                    javaTypes
                    |> Seq.map (fun jt -> 
                           if not <| (jt.Value :?> System.String).StartsWith("java.") then 
                               ConvertNamingConvention(jt.Value :?> System.String)
                           else (jt.Value :?> System.String))
                    |> Seq.map (fun strJt -> new CustomAttributeArgument((javaTypes |> Seq.head).Type, strJt))
                // Replace the Attribute arguments with the new ones
                implAtt.ConstructorArguments.Clear()
                implAtt.ConstructorArguments.Add(new CustomAttributeArgument(arg.Type, newJavaTypes.ToArray()))
    
    let rec ProcessType(typ : TypeDefinition) = 
        if not (typ.Name = "WindowsDirectory") && not (typ.Name = "NativePosixUtil") then 
            let classCondition (t : TypeDefinition) = 
                not t.IsRuntimeSpecialName && t.Name <> "<Module>" && t.Name <> "Resources" && t.IsPublic
            let nestedClassCondition (t : TypeDefinition) = not t.IsRuntimeSpecialName && t.IsNestedPublic
            // Replace the Class's namespace and 'Implements' attribute with the FlexLucene version
            //typ.Namespace <- ConvertNamingConvention(typ.Namespace)
            //typ |> regenerateImplementsAtt
            // Change the Method and Field names to FlexLucene convention
            if classCondition typ || nestedClassCondition typ then 
                if (not typ.IsInterface || not typ.IsAbstract) && typ.IsPublic then 
                    // Change field names
                    typ.Fields
                    |> Seq.filter (fun x -> x.IsPublic)
                    |> Seq.iter (fun x -> x.Name <- ConvertNamingConvention(x.Name))
                    // Change method names
                    ProcessMethods(typ)
            // Process all nested types (public, internal, etc)
            typ.NestedTypes |> Seq.iter ProcessType
    
    /// <summary>
    /// Renames all the Java methods to .net style naming convention
    /// </summary>
    let regenerateMethodNames() = 
        md <- Mono.Cecil.ModuleDefinition.ReadModule(OutputDirectory <!!> "FlexLucene.dll")
        editorBrowsableCtor <- md.Import(typeof<EditorBrowsableAttribute>.GetConstructor(Type.EmptyTypes))
        obsoleteCtor <- md.Import(typeof<ObsoleteAttribute>.GetConstructor(Type.EmptyTypes))
        editorStateRef <- md.Import(typeof<EditorBrowsableState>)
        md.Types |> Seq.iter ProcessType
        md.Write(OutputDirectory <!!> "FlexLucene.dll")

/// <summary>
/// Executes PEVerify on the final dll to ensure that the right IL is generated.
/// </summary>
let executePEVerify() = 
    !>>"Copying ikvm dlls to the output folder"
    loopFiles (RootDirectory <!!> "ikvm") |> Seq.iter (fun f -> File.Copy(f, OutputDirectory <!!> Path.GetFileName(f)))
    Exec(RootDirectory <!!> "PEVerify.exe", @"work\output\FlexLucene.dll")

/// <summary>
/// Execute all smoke tests
/// </summary>
let runSmokeTests() = 
    !>>"Starting Smoke Tests"
    Exec(RootDirectory <!!> "SmokeTests.bat", "")

/// <summary>
/// Copies the final artifact to the artifact directory at the root
/// </summary>
let copyArtifacts() = 
    let artifactDirectory = Directory.CreateDirectory(RootDirectory <!!> "Artifacts").ToString()
    File.Copy(OutputDirectory <!!> "FlexLucene.dll", artifactDirectory <!!> "FlexLucene.dll", true)

/// <summary>
/// Tasks which needs to executed as part of the build process
/// </summary>
let tasks = 
    [ //codecsCompilation, "FlexSearch.Codecs compilation phase"
      copyLuceneFilesToTarget, "Copy Lucene files to be compiled"
      extractMetaInformation, "Extract meta information from packages"
      generateMetaInformation, "Generate new meta data information"
      copyLibraries, "Copy Library files"
      executeProguard, "Execute Proguard"
      generateRenameMapping, "Generating mapping information"
      executeProguardPass2, "Execute ProGuard Pass - 2"
      executeIkvm, "Execute IKVM"
      addBuildInformation, "Add build information"
      CecilWriter.regenerateMethodNames, "Regenerate Method names"
      executePEVerify, "Execute PEVerify"
      copyArtifacts, "Copy artifacts to the Artifacts directory"
      runSmokeTests, "Run Smoke Tests" ]

tasks |> Seq.iter (fun t -> 
             let (task, desc) = t
             brk()
             !>(sprintf "TASK: %s" desc)
             task()
             brk())
!>"Build Finished."
