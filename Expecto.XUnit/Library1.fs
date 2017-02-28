namespace Expecto.XUnit

open Xunit.Sdk
open Expecto
open Expecto.Impl
open System.Reflection
open Expecto.Test
open Xunit.Abstractions
open FSharp.Control.Tasks.ContextInsensitive
open Xunit
open System
open System.IO
open System.Collections.Generic
open System.Threading.Tasks
open System.Diagnostics

[<Serializable>]
type ExpectoTestCase(test:FlatTest, testMethod:ITestMethod) =
    
    inherit LongLivedMarshalByRefObject()

    do printfn "ExpectoTestCase ctor"

    let mutable test = test
    let mutable testMethod = testMethod
    let mutable sourceInformation = null

    [<NonSerialized>]
    let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

    let serialize v =
        use ms = new MemoryStream()
        serializer.Serialize(ms, v)
    let deserialize (bytes:byte[]) =
        use ms = new MemoryStream(bytes)
        serializer.Deserialize(ms)

    new() = ExpectoTestCase(Unchecked.defaultof<FlatTest>, Unchecked.defaultof<ITestMethod>)

    interface IXunitTestCase with
        member self.Method = testMethod.Method
        member self.RunAsync(diagnosticMessageSink, messageBus, constructorArguments, aggregator, cancellationTokenSource) = task {

            let sw = Stopwatch.StartNew()
            let testCase = TestCase (test.test, if test.focusOn then Focused else Normal)
            let! res = Task.Run(fun () -> runTests ExpectoConfig.defaultConfig testCase)
            let passed = res = 0
            let elapsed = sw.Elapsed

            let summary = new RunSummary()
            summary.Failed <- if passed then 0 else 1
            summary.Skipped <- 0
            summary.Time <- decimal 1.0
            summary.Total <- 1
            return summary
        }
    interface IXunitSerializable with
        member self.Deserialize(info) =
            printfn "Deserialize"
//            failwith "not implmented: ExpectoTestCase.Deserialize"
            test <- info.GetValue<byte[]>("test") |> deserialize
            testMethod <- info.GetValue<byte[]>("testMethod") |> deserialize
        member self.Serialize(info) =
            printfn "Serialize"
//            failwith "not implmented: ExpectoTestCase.Serialize"
            info.AddValue("test", serialize test)
            info.AddValue("testMethod", serialize testMethod)
    interface ITestCase with
        member self.DisplayName = test.name
        member self.SkipReason = defaultArg test.shouldSkipEvaluation null
        member self.SourceInformation
            with get() = sourceInformation
            and set v = sourceInformation <- v
        member self.TestMethod = testMethod
        member self.TestMethodArguments = Array.empty
        member self.Traits = new Dictionary<_,_>()
        member self.UniqueID = Guid.NewGuid().ToString()



type ExpectoDiscoverer(diagnosticMessageSink:IMessageSink) =

    member x.Discover(discoveryOptions, testMethod, factAttribute) = 

        if false then
            new XunitTestCase(diagnosticMessageSink, TestMethodDisplay.Method, testMethod) :> IXunitTestCase
            |> Seq.singleton
        else
//            failwith "hello world!"
//            let msg = { new obj() with override x.ToString() = "Hello World"
//                        interface IMessageSinkMessage }
            let printDiag =
                ((+) "ExpectoDiscoverer.Discover: ") >> DiagnosticMessage >> diagnosticMessageSink.OnMessage >> ignore
//            Debugger.Launch() |> ignore
            printDiag "Starting"

            // ignore the attribute, get the assembly where the method is defined and get all expecto test cases from there
            let testAssemblyPath = testMethod.TestClass.Class.Assembly.AssemblyPath
            printDiag <| "testAssemblyPath is " + testAssemblyPath
            let tests = testFromAssembly (Assembly.LoadFrom testAssemblyPath)
            try
                match tests with
                | Some t ->
                    printDiag "testFromAssembly was Some"
                    try
                        [|
                            let flatList = toTestCodeList t
                            for t in flatList do
                                printDiag <| sprintf "One testcase: %s" t.name
                                yield ExpectoTestCase(t, testMethod) :> IXunitTestCase
                        |] :> seq<_>
                    with
                        exn ->
                        printDiag <| sprintf "Exception during discovery: %O" exn
                        reraise()
                | None ->
                    printDiag "testFromAssembly was None"
                    Seq.empty
            finally
                printDiag "Finished Discover"
                    
    interface IXunitTestCaseDiscoverer with
        member x.Discover(discoveryOptions, testMethod, factAttribute) =
            let seq =
                x.Discover(discoveryOptions, testMethod, factAttribute)
                |> Seq.toArray
            printfn "Got %i testcases" seq.Length
            seq :> seq<_>
        



[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("Expecto.XUnit.ExpectoDiscoverer", "Expecto.XUnit")>]
type ExpectoBridgeAttribute() =
    inherit TheoryAttribute()
