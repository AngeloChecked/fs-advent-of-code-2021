module AotTwoTest 

open Xunit
open AdventOfCode2021.Utils
open AdventOfCode2021.DayTwo

let aotInputFile = $"{__SOURCE_DIRECTORY__}/resource/day2.txt"

[<Fact>]
let ``parse command`` () =
    let inputs:List<string> = [
                  "forward 5"
                  "down 5"
                  "forward 8"
                  "up 3"
                  "down 8"
                  "forward 2"
                 ]

    let expect:List<Command> = [
                  Forward 5
                  Down 5
                  Forward 8
                  Up 3
                  Down 8
                  Forward 2
                 ]

    let parsedInputs:List<Command> = inputs |> List.map parseComand
    Assert.Equal<List<Command>>(expect, parsedInputs)


[<Fact>]
let ``solution Aot2`` () =
    let inputs = read aotInputFile |> Seq.toList
    let commandInputs:List<Command> = inputs |> List.map parseComand

    let solution = solutionAot2 inputs
    Assert.Equal(1962940, solution)

[<Fact>]
let ``solution Aot2 Part2`` () =
    let inputs = read aotInputFile |> Seq.toList
    let commandInputs:List<Command> = inputs |> List.map parseComand

    let solution = solutionAot2Part2 inputs
    Assert.Equal(1813664422, solution)