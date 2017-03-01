namespace Expecto.XUnit.Test

open Expecto

module Tests =
    open System.Threading

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

    [<Tests>]
    let tests3 =
      test "A long test" {
        Thread.Sleep(10000)
      }
