namespace AdventOfCode2021

module DayOne =
    let solutionAot1 (inputs: seq<string>) = 
        inputs 
        |> Seq.map int
        |> Seq.pairwise
        |> Seq.filter(fun (a, b) -> b > a )
        |> Seq.length 

    let solutionAot1Part2 (inputs: seq<string>) = 
        inputs 
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter(fun (a, b) -> b > a )
        |> Seq.length 