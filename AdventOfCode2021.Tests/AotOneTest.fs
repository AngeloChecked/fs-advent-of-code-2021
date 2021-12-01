module AotOneTest 

open Xunit
open AdventOfCode2021.DayOne
open AdventOfCode2021.Utils

let aotInputFile = $"{__SOURCE_DIRECTORY__}/resource/day1.txt"

[<Fact>]
let ``Aot 2021 solution`` () =
    let inputs = read aotInputFile 
    Assert.Equal(1374, solutionAot1 inputs)