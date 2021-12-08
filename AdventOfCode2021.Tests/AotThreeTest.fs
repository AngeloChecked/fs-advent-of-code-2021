module AotThreeTest 

open Xunit
open AdventOfCode2021.Utils
open AdventOfCode2021.DayThree

let aotInputFile = $"{__SOURCE_DIRECTORY__}/resource/day3.txt"

[<Fact>]
let ``parse inputs`` () =
    let inputs = [
                    "00100"
                    "11110"
                    "10110"
                    "10111"
                    "10101"
                    "01111"
                    "00111"
                    "11100"
                    "10000"
                    "11001"
                    "00010"
                    "01010"
                ]   
    let expect = [
                    [0;0;1;0;0]
                    [1;1;1;1;0]
                    [1;0;1;1;0]
                    [1;0;1;1;1]
                    [1;0;1;0;1]
                    [0;1;1;1;1]
                    [0;0;1;1;1]
                    [1;1;1;0;0]
                    [1;0;0;0;0]
                    [1;1;0;0;1]
                    [0;0;0;1;0]
                    [0;1;0;1;0]
                ]   
    let actual = linesToMatrix inputs
    Assert.Equal<List<List<int>>>(expect, actual)

[<Fact>]
let ``most common number in matrix column`` () =
    let matrix = [
                    [0;0;1;0;0]
                    [1;1;1;1;0]
                    [1;0;1;1;0]
                    [1;0;1;1;1]
                    [1;0;1;0;1]
                    [0;1;1;1;1]
                    [0;0;1;1;1]
                    [1;1;1;0;0]
                    [1;0;0;0;0]
                    [1;1;0;0;1]
                    [0;0;0;1;0]
                    [0;1;0;1;0]
                ]   
    let actual = mostCommonPerMatrixColumn matrix 
    Assert.Equal<List<int>>([1;0;1;1;0], actual)

[<Fact>]
let ``invert row`` () =
    let row = [1;0;1;1;0]   
    let actual = invertRow row 
    Assert.Equal<List<int>>([0;1;0;0;1], actual)

[<Fact>]
let ``from binary row to decimal`` () =
    Assert.Equal(9, fromBinaryToDecimal [0;1;0;0;1])
    Assert.Equal(90, fromBinaryToDecimal [1;0;1;1;0;1;0])
    Assert.Equal(22, fromBinaryToDecimal [1;0;1;1;0])

[<Fact>]
let ``AOT3 solution`` () =
    let inputs = read aotInputFile |> Seq.toList 
    let matrix = linesToMatrix inputs
    let binaryGammaRate = mostCommonPerMatrixColumn matrix 
    
    let epsilonRate = binaryGammaRate |> invertRow |> fromBinaryToDecimal 
    let gammaRate = binaryGammaRate |> fromBinaryToDecimal 

    Assert.Equal(1071734, epsilonRate * gammaRate)

[<Fact>]
let ``oxygen criteria`` () =
    let matrix = [
                    [0;0;1;0;0]
                    [1;1;1;1;0]
                    [1;0;1;1;0]
                    [1;0;1;1;1]
                    [1;0;1;0;1]
                    [0;1;1;1;1]
                    [0;0;1;1;1]
                    [1;1;1;0;0]
                    [1;0;0;0;0]
                    [1;1;0;0;1]
                    [0;0;0;1;0]
                    [0;1;0;1;0]
                ]   
    let oxygenRating = filterPerCriteria oxigenGeneratorCriteria matrix 
    Assert.Equal<List<int>>([1;0;1;1;1], oxygenRating)

[<Fact>]
let ``AOT3 solution Part2`` () =
    let inputs = read aotInputFile |> Seq.toList 
    let matrix = linesToMatrix inputs
    let oxygenRating = filterPerCriteria oxigenGeneratorCriteria matrix |> fromBinaryToDecimal
    let co2Rating = filterPerCriteria co2ScrubberCriteria matrix |> fromBinaryToDecimal 
    let lifeSupportRating = oxygenRating * co2Rating
    
    Assert.Equal(6124992, lifeSupportRating)
