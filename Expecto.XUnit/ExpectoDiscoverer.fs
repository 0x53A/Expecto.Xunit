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

    // TODO: clean up the whole (de)serialization stuff
    let mutable test = test
    let mutable testMethod = testMethod
    let mutable testName = if obj.ReferenceEquals(null, test) then "" else test.name
    let mutable shouldSkip = if obj.ReferenceEquals(null, test) then "" else defaultArg test.shouldSkipEvaluation null
    let mutable sourceInformation = null
    let mutable n = n

    let uniqueId = sprintf "Expecto-%s-%i" testName n

    new() = ExpectoTestCase(Unchecked.defaultof<FlatTest>, Unchecked.defaultof<ITestMethod>, 0)

    interface IXunitSerializable with
        member self.Deserialize(info) =
            testMethod <- info.GetValue("testMethod")
            testName <- info.GetValue("testName")
            shouldSkip <- info.GetValue("shouldSkip")
            n <- info.GetValue("n")

        member self.Serialize(info) =
            info.AddValue("testMethod", testMethod)
            info.AddValue("testName", testName)
            info.AddValue("shouldSkip", shouldSkip)
            info.AddValue("n", n)

    interface IXunitTestCase with
        member self.Method = testMethod.Method
        member self.RunAsync(diagnosticMessageSink, messageBus, constructorArguments, aggregator, cancellationTokenSource) =
        
          let xunitTest = XunitTest(self, testName)
          let queueMsg (msg:#IMessageSinkMessage) =
              messageBus.QueueMessage (msg) |> ignore
          
          queueMsg(TestCaseStarting self)
          queueMsg(TestStarting xunitTest)

          Task.Run (fun () ->            
            
            let sw = Stopwatch.StartNew()
            let test =
                if obj.ReferenceEquals(null, test) then
                    // need to rediscover:
                    // this object was passed through serialization,
                    // and the expecto test object is not serializable.
                    // I assume that the order the tests are returned by is stable, so I re-enumerate the tests and get the n-th item

                    let testAssemblyPath = testMethod.TestClass.Class.Assembly.AssemblyPath
                    let tests = testFromAssembly (Assembly.LoadFrom testAssemblyPath)
                    let flatList = toTestCodeList tests.Value
                    flatList |> Seq.item n
                else
                    test
            let testCase = TestCase (test.test, if test.focusOn then Focused else Normal)
            let mutable summary : Choice<TestRunSummary, exn> option = None
            try
                let config =
                    let summaryFn = fun cfg (smr:TestRunSummary) ->
                        summary <- Some (Choice1Of2 smr)
                        Async.AwaitTask <| Task.FromResult ()
                    { ExpectoConfig.defaultConfig
                        with printer = { TestPrinters.silent with summary = summaryFn } }

                let x = runTests config testCase
                x |> ignore
            with
              exn -> summary <- Some (Choice2Of2 exn)
              
            let elapsed = decimal sw.Elapsed.TotalSeconds
            match summary with
            | None ->
                let msg = "expecto didn't produce a result"
                queueMsg (TestFailed (xunitTest, elapsed, null, Exception msg))
                RunSummary(Failed=1, Total=1, Time=elapsed)
            | Some (Choice2Of2 exn) ->
                queueMsg (TestFailed (xunitTest, elapsed, null, exn))
                RunSummary(Failed=1, Total=1, Time=elapsed)
            | Some (Choice1Of2 testRun) ->
                match testRun.results with
                | head :: [] ->
                    let headResult = snd head
                    match headResult.result with
                    | Passed     -> queueMsg (TestPassed (xunitTest, elapsed, null))
                    | Ignored s  -> queueMsg (TestSkipped (xunitTest, s))
                    | Failed s   -> queueMsg (TestFailed (xunitTest, elapsed, null, Exception s))
                    | Error exn  -> queueMsg (TestFailed (xunitTest, elapsed, null, exn))
                    RunSummary(Total=testRun.results.Length,
                        Failed=testRun.errored.Length+testRun.failed.Length,
                        Skipped=testRun.ignored.Length,
                        Time=elapsed)
                | []
                | _ ->
                    let msg = sprintf "Expecto did not produce the expected number of results. Expected 1, got %i" testRun.results.Length
                    queueMsg (TestFailed (xunitTest, elapsed, msg, null))
                    RunSummary(Total=testRun.results.Length,
                        Failed=testRun.errored.Length+testRun.failed.Length,
                        Skipped=testRun.ignored.Length,
                        Time=elapsed)
        )

    interface ITestCase with
        member self.DisplayName = testName
        member self.SkipReason = shouldSkip
        member self.SourceInformation
            with get() = sourceInformation
            and set v = sourceInformation <- v
        member self.TestMethod = testMethod
        member self.TestMethodArguments = Array.empty
        member self.Traits = new Dictionary<_,_>()
        member self.UniqueID = uniqueId



type ExpectoDiscoverer(diagnosticMessageSink:IMessageSink) =

    // copy-pasted from https://github.com/haf/expecto/blob/5d06334f2aa5db21971a37423294ca4a0b7cf1d3/Expecto/Expecto.fs#L1203
    let getTypeAndMethod asm testCode =
      match testCode with
      | Sync test ->
        let t = getFuncTypeToUse test asm
        let m = t.GetTypeInfo().GetMethods () |> Seq.find (fun m -> (m.Name = "Invoke") && (m.DeclaringType = t))
        (t, m)
      | Async _ | AsyncFsCheck _ ->
        (null, null)


    interface IXunitTestCaseDiscoverer with
        member x.Discover(discoveryOptions, testMethod, factAttribute) =
            let printDiag =
                ((+) "[ExpectoDiscoverer.Discover]: ") >> DiagnosticMessage >> diagnosticMessageSink.OnMessage >> ignore

            printDiag "Starting"

            // ignore the attribute, get the assembly where the method is defined and get all expecto test cases from there
            let testAssemblyPath = testMethod.TestClass.Class.Assembly.AssemblyPath
            printDiag <| "testAssemblyPath is " + testAssemblyPath
            let asm = Assembly.LoadFrom testAssemblyPath
            let tests = testFromAssembly (asm)
            match tests with
            | Some t -> seq {            
                let flatList = toTestCodeList t
                // note: for now the "identity" of the test is the index from the expecto iteration - this should be changed to the string-testname
                for i, t in flatList |> Seq.mapi (fun i t -> i,t) do
                    printDiag <| "Discovered one: " + t.name
                    
                    //let method = getMethodName asm t.test
                    //let location = getLocation asm t.test
                    
                    // get test method (this will be in the compiler-generated type that holds the lambda - there is no actual test class in the source code)
                    let typ,meth = getTypeAndMethod asm t.test
                    match typ,meth with
                    | null, null ->
                        // we couldn't get more information -> attach it to the original dummy method
                        yield ExpectoTestCase(t, testMethod, i) :> IXunitTestCase
                    | typ,meth ->
                        let reflectionTypInfo = Reflector.Wrap(typ)
                        // this is highly-probably ".Invoke" in the compiler-generated class for the lambda
                        let m = reflectionTypInfo.GetMethod(meth.Name, true)
                        let iTypInfo = testMethod.TestClass.TestCollection.TestAssembly.Assembly.GetType(typ.FullName)
                        let testClass = TestClass(testMethod.TestClass.TestCollection, iTypInfo)
                        yield ExpectoTestCase(t, TestMethod(testClass, m), i) :> IXunitTestCase
                printDiag "Finished"
              }
            | None ->
                Seq.empty

/// I didn't find a way to register an Assembly-Discoverer with xunit (there may well be, please tell me)
/// So instead the user should create one dummy method and attach this attribute to it.
/// This will trigger my custom 'IXunitTestCaseDiscoverer', which will return all expecto tests in the whole assembly.
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("Expecto.XUnit.ExpectoDiscoverer", "Expecto.XUnit")>]
type ExpectoBridgeAttribute() =
    inherit FactAttribute()
