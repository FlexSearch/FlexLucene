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
let LuceneVersion = "6.3.0"
let LuceneFullVersion = "6.3.0.0"
let FileVersion = "6.3.0.0"

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
    let getName(input : string) = 
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
            if elem.StartsWith("org.apache") || elem.StartsWith("Flex") then getName elem :: acc
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
       sprintf "lucene-analyzers-kuromoji-%s" LuceneVersion
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
       sprintf "lucene-classification-%s" LuceneVersion
       sprintf "lucene-spatial-%s" LuceneVersion
       sprintf "lucene-spatial3d-%s" LuceneVersion
       sprintf "lucene-spatial-extras-%s" LuceneVersion
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
                                             else if entry.FullName.StartsWith("META-INF") 
                                                  && not <| entry.FullName.EndsWith("RSA") 
                                                  && not <| entry.FullName.EndsWith("SF") 
                                                then 
                                                    !>> (sprintf "Adding META-INF entry to the jar:  %s" entry.FullName)
                                                    entry.FullName :: acc
                                             else acc) [] archive.Entries
                                     toBeDeleted |> Seq.iter (fun res -> archive.GetEntry(res).Delete()))

/// <summary
/// Add resource files to the FlexLucene.* classpath as well
/// </summary>
let addResourceFilesToFlexLuceneClassPath() =
    // Create a temporary directory to store the files to copy
    let tmpDir = CreateAndEmptyDirectory( TempDirectory <!!> "Resources")

    let capitalizeAfterSlash (str : string) =
        let finalSlash = str.LastIndexOf('/')
        let chars = str.ToCharArray()
        [1..chars.Length]
        |> Seq.iter (fun i -> if chars.[i-1] = '/' && i-1 <> finalSlash
                              then chars.[i] <- Char.ToUpper chars.[i])

        new string(chars)

    let generateFlexLuceneResource (entry : ZipArchiveEntry)  =
        let fn = tmpDir <!!> (Guid.NewGuid()).ToString()
        entry.ExtractToFile fn
        let newPath = entry.FullName.Replace(@"org/apache/lucene", "FlexLucene")
                      |> capitalizeAfterSlash
        !> (sprintf "orig: %s;  new: %s" entry.FullName newPath)
        (fn, newPath)

    loopFiles LuceneDirectory
    |> Seq.iter (fun file ->
            use archive = ZipFile.Open(file, ZipArchiveMode.Update) 

            archive.Entries
            |> Seq.filter (fun e -> e.Name.EndsWith(".txt") || e.Name.EndsWith(".dat"))
            |> Seq.map generateFlexLuceneResource
            |> Seq.toList
            |> Seq.iter (archive.CreateEntryFromFile >> ignore))
            
        

/// <summary>
/// Builds FlexLucene compatible meta data information
/// </summary>
let generateMetaInformation() = 
    loopDir TempDirectory |> Seq.iter (fun dir -> 
                                 loopFiles dir |> Seq.iter (fun file -> 
                                                      let dirInfo = new DirectoryInfo(Path.GetDirectoryName(file))
                                                      let targetFileName = getName(dirInfo.Name)
                                                      let targetPath = ServicesDirectory <!!> targetFileName
                                                      File.AppendAllLines(targetPath, CreateServiceFile(file))))
    !>"Create meta data jar"
    ZipFile.CreateFromDirectory(MetaDirectory, LuceneDirectory <!!> "Metadata.jar")

/// <summary>
/// Copies related libraries which are needed for compilation
/// </summary>
let copyLibraries() = 
    loopFiles LibSrcDirectory |> Seq.iter (fun f -> File.Copy(f, LuceneDirectory <!!> (Path.GetFileName(f))))

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
    patchExec (sprintf """%s /s copyright "(c) 2010-2016 FlexSearch" """ finalDllPath)

/// Generates the mapping information from the ProGuard mapping file
module Mapper =
    let memberSpaces = "    "
    type MemberMapping =
        {
            mutable Name : string
            ReturnType : string
            Signature : string
            IsField : bool
            HasParameters : bool
            OverrideName : bool
        }
        member this.ToProGuardFormat() =
            sprintf "%s%s -> %s" memberSpaces this.Signature (if this.OverrideName then 
                                                                getName(this.Name) 
                                                              else this.Name)

    type TypeMapping =
        {
            Name : string
            OverrideName : bool
            Members : ResizeArray<MemberMapping>
        }
        member this.ToProGuardFormat() =
            sprintf "%s -> %s:" this.Name (if this.OverrideName then getName(this.Name) else this.Name)

    /// Generates the new mapping information for renaming classes in accordance 
    /// with C# naming convention 
    let generateRenameMapping() =
        let inputFile = WorkDirectory <!!> "mapping-out.txt"
        let lines = File.ReadAllLines(inputFile)
        let memberSpaces = "    "
        let mappings = new ResizeArray<TypeMapping>()
         
        let split(input : string) = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        
        /// Process a type from the incoming mapping file line
        let processMember(parts : string[]) = 
            assert(parts.Length = 4)
            let mutable isField = false
            let hasParameters = not <| parts.[1].Contains("()")
            let name = 
                // It is a method declaration
                if parts.[1].Contains("(") then
                    parts.[1].Substring(0, parts.[1].IndexOf("("))
                else 
                    isField <- true
                    parts.[1]
            let overideName = not (name = parts.[3] || name.Contains("<"))
            let memberMapping =
                {
                    Name = name
                    ReturnType = parts.[0]
                    Signature = sprintf "%s %s" parts.[0] parts.[1]
                    IsField = isField
                    HasParameters = hasParameters
                    OverrideName = overideName
                }
            mappings.Last().Members.Add(memberMapping)
        
        /// Process a member from the incoming mapping file line
        let processType(parts : string[]) =
            assert(parts.Length = 3)
            // Get rid of the : in the end
            let name = parts.[2].Substring(0, parts.[2].Length - 1)
            // The before and after class names are same so
            // must be one of the classes which can't be renamed.
            let overideName = not (parts.[0] = name)
            let typeMapping =
                {
                    Name = parts.[0]
                    OverrideName = overideName
                    Members = new ResizeArray<MemberMapping>()
                }
            mappings.Add(typeMapping)    
             
        let processLine(line : string) =
            let parts = split line
            // Check if the line contains a class declaration
            if line.StartsWith(memberSpaces) then
               processMember parts
            else
                processType parts
        
        let validateTypeMapping(mapping : TypeMapping) =
            // Fins all the instances where a field and a method have the same name 
            mapping.Members
            |> Seq.groupBy (fun g -> g.Name)
            |> Seq.filter (fun (_,items) -> items.Count() > 1 && items |> Seq.exists(fun i -> i.IsField))
            |> Seq.iter (fun (_,items) -> 
                items 
                |> Seq.filter(fun i -> i.IsField = true && i.OverrideName = true) 
                |> Seq.iter(fun i -> 
                    printfn "[INFO] Renaming due to conflict: %s -> %s" mapping.Name i.Signature 
                    i.Name <- "_" + getName i.Name))

        let generateMappingFile() =
            let output = new ResizeArray<string>()
            for typeMapping in mappings do 
                validateTypeMapping typeMapping
                output.Add(typeMapping.ToProGuardFormat())
                typeMapping.Members |> Seq.iter(fun i -> output.Add(i.ToProGuardFormat()))
            output

        lines |> Array.iter processLine
        File.WriteAllLines(WorkDirectory <!!> "mapping-in.txt", generateMappingFile())

// ---------------------------------------------
// Mono Cecil based rewrite section
// ---------------------------------------------
module CecilWriter = 
    open System.Runtime.CompilerServices
    let mutable md : ModuleDefinition = Unchecked.defaultof<_>
    let mutable editorBrowsableCtor : MethodReference = Unchecked.defaultof<_>
    let mutable methodImplCtor : MethodReference = Unchecked.defaultof<_>
    let mutable editorStateRef : TypeReference = Unchecked.defaultof<_>
    let mutable methodImplRef : TypeReference = Unchecked.defaultof<_>
    
    // Custom attribute to stop method from showing in intellisense
    let GetEditorBrowsableAttr() = 
        let attr = new CustomAttribute(editorBrowsableCtor)
        attr.Properties.Add
            (new CustomAttributeNamedArgument("EditorBrowsableState", new CustomAttributeArgument(editorStateRef, 1)))
        attr
   
    let getAggressiveInliningAttr() =
        let attr = new CustomAttribute(methodImplCtor)
        attr.ConstructorArguments.Add(new CustomAttributeArgument(methodImplRef, MethodImplOptions.AggressiveInlining))
        attr

    let ProcessMethods(typ : TypeDefinition) = 
        let newMethods = new ResizeArray<MethodDefinition>()

        let hasEditorBrowsableAttribute(meth : MethodDefinition) =
            meth.CustomAttributes
            |> Seq.exists(fun x -> x.GetType() = typeof<EditorBrowsableAttribute>)

        let specialMethodNames = ["toString"; "hashCode"; "equals" ]

        /// Generate a method which is calls the passed method and uses .net style 
        /// naming convention.
        /// This must be a method from the java.lang package as we were not able to rename it 
        /// using Proguard. Let's inject a new method which will shadow this method and will have
        /// proper case.    
        ///
        /// System.Object's virtual ToString(), GetHashCode() and Equals() methods are hidden (using 'new' keyword)
        /// by the implementation of java.lang.Object. Therefore, any Lucene class that implements java.lang.Object 
        /// cannot override these methods. You can only further hide them using the 'new' keyword.
        ///
        /// If you want to override one of these methods from the generated FlexLucene classes then you have to
        /// override the methods they are calling (i.e. toString(), hashCode() and equals(), respectively). 
        /// e.g. override toString() = base.toString() + "<overridden>"
        let addShadowMethod(meth : MethodDefinition) =
            printfn "[INFO] Adding a shadow method for the method: %s" meth.FullName
            let methAttributes = 
                if specialMethodNames.Contains(meth.Name) then
                    // We don't want to make the generated method virtual because that will have a performance impact
                    // http://stackoverflow.com/questions/667634/what-is-the-performance-cost-of-having-a-virtual-method-in-a-c-class
                    MethodAttributes.Public ||| MethodAttributes.HideBySig //||| MethodAttributes.Virtual ||| MethodAttributes.CheckAccessOnOverride
                else
                    MethodAttributes.Public
            
            let newMeth = new MethodDefinition(getName(meth.Name), methAttributes, meth.ReturnType)

            // http://comments.gmane.org/gmane.comp.java.ikvm.devel/2463
            if meth.Name = "hashCode" then newMeth.Name <- "GetHashCode"
            
            // Add the necessary parameters
            meth.Parameters |> Seq.iter newMeth.Parameters.Add

            newMeth.CustomAttributes.Add <| getAggressiveInliningAttr()

            newMeth.Body <- new MethodBody(newMeth)
            let ilProcessor = newMeth.Body.GetILProcessor()
            ilProcessor.Append(ilProcessor.Create(OpCodes.Ldarg_0))
            if meth.Parameters.Count > 0 then ilProcessor.Append(ilProcessor.Create(OpCodes.Ldarg_1))
            if meth.Parameters.Count > 1 then ilProcessor.Append(ilProcessor.Create(OpCodes.Ldarg_2))
            if meth.Parameters.Count > 2 then ilProcessor.Append(ilProcessor.Create(OpCodes.Ldarg_3))
            // TODO if we have more than 3 parameters in a method, we will need to use OpCodes.Ldarg_S
            ilProcessor.Append(ilProcessor.Create(OpCodes.Callvirt, meth))
            ilProcessor.Append(ilProcessor.Create(OpCodes.Ret)) 
            newMethods.Add(newMeth)

        for meth in typ.Methods do
            // Remove the no in-line attribute from IKVM generated code as it is due to a
            // .net 2.0 related issue
            meth.NoInlining <- false
            meth.NoOptimization <- false
            if meth.Name <> null then
                let startsWith = meth.Name.ToCharArray().[0]
                // Check if the method has EditorBrowsable attribute. If yes then we don't want to replicate the method as it is not
                // intedend for use anyhow.
                let implAtts = 
                    meth.CustomAttributes 
                    |> Seq.filter (fun a -> a.AttributeType.Name =  typeof<EditorBrowsableAttribute>.Name)
                    |> Seq.length

                if not meth.IsRuntimeSpecialName 
                    && not meth.IsSpecialName 
                    && not meth.IsConstructor 
                    && not meth.IsNative 
                    && not meth.IsAssembly 
                    && not meth.IsPInvokeImpl 
                    && not meth.IsUnmanaged 
                    && meth.IsPublic
                    && not <| Char.IsUpper(startsWith) 
                    && startsWith <> '<' 
                    && implAtts = 0 then 
                        if meth.IsAbstract then
                            addShadowMethod(meth)
                        else if not meth.IsAbstract then
                            if specialMethodNames |> Seq.exists ((=) meth.Name) then
                                addShadowMethod(meth)
                                
                                // Make the original method non browsable from the IDE
                                meth.CustomAttributes.Add(GetEditorBrowsableAttr())
                            else
                                let newMeth = new MethodDefinition(getName(meth.Name), meth.Attributes, meth.ReturnType)
                                meth.Parameters |> Seq.iter (fun x -> newMeth.Parameters.Add(x))
                                newMeth.Body <- meth.Body
                                /// Copy all attributes from the existing method to the new method 
                                meth.CustomAttributes 
                                |> Seq.iter(fun x -> newMeth.CustomAttributes.Add(x))
                                newMethods.Add(newMeth)
                                // Make the method non browsable from the IDE
                                meth.CustomAttributes.Add(GetEditorBrowsableAttr())

        newMethods |> Seq.iter (fun x -> typ.Methods.Add(x))
        
    let rec ProcessType(typ : TypeDefinition) = 
        let classCondition (t : TypeDefinition) = 
            not t.IsSpecialName && t.Name <> "<Module>" && t.Name <> "Resources" && t.IsPublic
        let nestedClassCondition (t : TypeDefinition) = not t.IsSpecialName && t.IsNestedPublic

        // Change the Method and Field names to FlexLucene convention
        if classCondition typ || nestedClassCondition typ then 
            if (not typ.IsInterface || not typ.IsAbstract) && typ.IsPublic then 
                ProcessMethods(typ)
        // Process all nested types (public, internal, etc)
        typ.NestedTypes |> Seq.iter ProcessType
    
    /// <summary>
    /// Renames all the Java methods to .net style naming convention
    /// </summary>
    let regenerateMethodNames() = 
        let parameters = new ReaderParameters()
        parameters.ReadingMode <- ReadingMode.Immediate
        md <- Mono.Cecil.ModuleDefinition.ReadModule(OutputDirectory <!!> "FlexLucene.dll", parameters)
        editorBrowsableCtor <- md.Import(typeof<EditorBrowsableAttribute>.GetConstructor(Type.EmptyTypes))
        editorStateRef <- md.Import(typeof<EditorBrowsableState>)
        methodImplCtor <- md.Import(typeof<MethodImplAttribute>.GetConstructor([| typeof<MethodImplOptions> |]))
        methodImplRef <- md.Import(typeof<MethodImplOptions>)
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
      addResourceFilesToFlexLuceneClassPath, "Add Resource files to FlexLucene class path"
      copyLibraries, "Copy Library files"
      executeProguard, "Execute Proguard"
      Mapper.generateRenameMapping, "Generating mapping information"
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
