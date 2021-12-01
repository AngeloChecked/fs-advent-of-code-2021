module Tests

open Xunit
open AdventOfCode2021.Utils

let aotInputFile = $"{__SOURCE_DIRECTORY__}/resource/day1.txt"

[<Fact>]
let ``Read File`` () =
    Assert.NotEmpty(read aotInputFile)



