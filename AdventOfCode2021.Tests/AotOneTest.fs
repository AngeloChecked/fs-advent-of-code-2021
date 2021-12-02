module AotOneTest 

open Xunit
open AdventOfCode2021.DayOne
open AdventOfCode2021.Utils

let aotInputFile = $"{__SOURCE_DIRECTORY__}/resource/day1.txt"

[<Fact>]
let ``Aot 2021 solution`` () =
    let inputs = read aotInputFile 
    Assert.Equal(1374, solutionAot1 inputs)

[<Fact>]
let ``Aot 2021 solution, part 2`` () =
    let inputs = read aotInputFile 
    Assert.Equal(1418, solutionAot1Part2 inputs)

// [<Fact>]
// let ``triplewise`` () =
//     let inputs =  [|1; 2; 3; 2; 3; 4;|]
//     let expect = [|[|1; 2; 3|]; [|2; 3; 2|]; [|3; 2; 3|]; [|2; 3; 4|]|]
//     Assert.Equal(expect, (Array.windowed 0 inputs))

