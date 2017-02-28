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
type ExpectoTestCase(test:FlatTest, testMethod:ITestMethod, n:int) =
    
    inherit LongLivedMarshalByRefObject()

    do printfn "ExpectoTestCase ctor"

    let mutable test = test
    let mutable testMethod = testMethod
    let mutable testName = if obj.ReferenceEquals(null, test) then "" else test.name
    let mutable shouldSkip = if obj.ReferenceEquals(null, test) then "" else defaultArg test.shouldSkipEvaluation null
    let mutable sourceInformation = null
    let mutable n = n

    new() = ExpectoTestCase(Unchecked.defaultof<FlatTest>, Unchecked.defaultof<ITestMethod>, 0)

    interface IXunitTestCase with
        member self.Method = testMethod.Method
        member self.RunAsync(diagnosticMessageSink, messageBus, constructorArguments, aggregator, cancellationTokenSource) = Task.Run (fun () ->
            
            let queueMsg (msg:#IMessageSinkMessage) =
                messageBus.QueueMessage (msg) |> ignore
            
            let sw = Stopwatch.StartNew()
            let test =
                if obj.ReferenceEquals(null, test) then
                    // need to rediscover
                    let testAssemblyPath = testMethod.TestClass.Class.Assembly.AssemblyPath
                    let tests = testFromAssembly (Assembly.LoadFrom testAssemblyPath)
                    let flatList = toTestCodeList tests.Value
                    flatList |> Seq.item n
                else
                    test
            let testCase = TestCase (test.test, if test.focusOn then Focused else Normal)
            let xunitTest = XunitTest(self, testName)
            queueMsg(TestStarting xunitTest)
            let mutable summary : TestRunSummary = Unchecked.defaultof<_>
            try
                let config =
                    let summaryFn = fun cfg (smr:TestRunSummary) ->
                        summary <- smr
                        Async.AwaitTask <| Task.FromResult ()
                    { ExpectoConfig.defaultConfig
                        with printer = { TestPrinters.silent with summary = summaryFn } }

                let x = runTests config testCase
                if x <> 0 then failwithf "%A" summary
                let elapsed = decimal sw.Elapsed.TotalSeconds
                queueMsg (TestPassed (xunitTest, elapsed, sprintf "%A" summary))
                RunSummary(Total=1, Time=elapsed)
            with
              exn ->
                let elapsed = decimal sw.Elapsed.TotalSeconds
                queueMsg (TestFailed (xunitTest, elapsed, sprintf "%A" summary, exn))
                RunSummary(Failed=1, Total=1, Time=elapsed)
        )
    interface IXunitSerializable with
        member self.Deserialize(info) =
            printfn "Deserialize"
            testMethod <- info.GetValue("testMethod")
            testName <- info.GetValue("testName")
            shouldSkip <- info.GetValue("shouldSkip")
            n <- info.GetValue("n")
//            sourceInformation <- info.GetValue("sourceInformation")
        member self.Serialize(info) =
            printfn "Serialize"
            info.AddValue("testMethod", testMethod)
            info.AddValue("testName", testName)
            info.AddValue("shouldSkip", shouldSkip)
            info.AddValue("n", n)
//            info.AddValue("sourceInformation", sourceInformation)
    interface ITestCase with
        member self.DisplayName = testName
        member self.SkipReason = shouldSkip
        member self.SourceInformation
            with get() = sourceInformation
            and set v = sourceInformation <- v
        member self.TestMethod = testMethod
        member self.TestMethodArguments = Array.empty
        member self.Traits = new Dictionary<_,_>()
        member self.UniqueID = sprintf "Expecto.XUnit-%i" n



type ExpectoDiscoverer(diagnosticMessageSink:IMessageSink) =

    member x.Discover(discoveryOptions, testMethod, factAttribute) = 

        if false then
            new XunitTestCase(diagnosticMessageSink, TestMethodDisplay.Method, testMethod) :> IXunitTestCase
            |> Seq.singleton
        else
            let printDiag =
                ((+) "ExpectoDiscoverer.Discover: ") >> DiagnosticMessage >> diagnosticMessageSink.OnMessage >> ignore
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
                            for i, t in flatList |> Seq.mapi (fun i t -> i,t) do
                                printDiag <| sprintf "One testcase: %s" t.name
                                yield ExpectoTestCase(t, testMethod, i) :> IXunitTestCase
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
