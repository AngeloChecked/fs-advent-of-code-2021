namespace AdventOfCode2021

module DayOne =
    let solutionAot1 (inputs: seq<string>) = 
        inputs 
        |> Seq.map int
        |> Seq.pairwise
        |> Seq.filter(fun (a, b) -> b > a )
        |> Seq.length 

