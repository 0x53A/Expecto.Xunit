namespace Expecto.XUnit.Test

module Tests =
    open Expecto
    open Xunit
    
    do
        MBrace.FsPickler.FsPickler.CreateBinarySerializer() |> ignore

    [<Fact>]
    let factDummy () = ()

    [<Expecto.XUnit.ExpectoBridge>]
    let dummy() = ()
    
    [<Tests>]
    let tests1 =
      test "A simple test" {
        let subject = "Hello World"
        Expect.equal subject "Hello World" "The strings should equal"
      }

    [<Tests>]
    let tests2 =
      test "A failing test" {
        let subject = "Hello world2"
        Expect.equal subject "Hello World" "The strings should equal"
      }
