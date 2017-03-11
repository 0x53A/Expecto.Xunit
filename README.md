# Expecto.Xunit


## How to use

Declare your tests normally with expecto, and add one dummy method with the Attribute:

```F#

open Expecto

module Tests =

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


```

![image](https://cloud.githubusercontent.com/assets/4236651/23439067/34aee5ae-fe15-11e6-9e24-59f6ec55674d.png)

![image](https://cloud.githubusercontent.com/assets/4236651/23439144/acb7854c-fe15-11e6-9b50-0f3cb479eb3b.png)

## Fixing no tests found

If the xunit runner doesn't find any tests, then the issue may be a missing (or incorrect) binding redirect for FSharp.Core.

Modify the app.config for the __unit test project__: https://fsharp.github.io/2015/04/18/fsharp-core-notes.html#use-binding-redirects-for-applications
(and make sure the version number is the one that is actually referenced by your project.)

