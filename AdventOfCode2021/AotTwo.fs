namespace AdventOfCode2021

module DayTwo =
    type Submarine = { Depth: int; Horizontal: int; Aim: int } 
    let baseSubmarine = { Depth=0; Horizontal=0; Aim=0} 

    type Command = Up of int 
                 | Down of int
                 | Forward of int 

    let parseCommand (line:string): Command = 
        let splitted = line.Split(' ') |> Array.toList
        match splitted with
        | ["up"; value] -> Up (int value)
        | ["down"; value] -> Down (int value)
        | ["forward"; value] -> Forward (int value)
        | _ -> failwith "impossible"

    let applyCommandToSubmarine {Depth=depth; Horizontal=horizontal} (command:Command) :Submarine = 
        match command with
        | Up value -> {Depth=depth-value; Horizontal=horizontal; Aim=0}
        | Down value -> {Depth=depth+value; Horizontal=horizontal; Aim=0}
        | Forward value -> {Depth=depth; Horizontal=horizontal+value; Aim=0}
        | _ -> failwith "impossible"

    let solutionAot2 inputs =
        let lastSubmarineState = 
            inputs 
            |> List.map parseCommand
            |> List.fold applyCommandToSubmarine baseSubmarine 
        let {Depth=depth; Horizontal=horizontal} = lastSubmarineState
        horizontal*depth

    let applyCommandToSubmarinePart2 {Depth=depth; Horizontal=horizontal; Aim=aim} (command:Command) :Submarine = 
        match command with
        | Up value -> {Depth=depth; Horizontal=horizontal; Aim=aim-value}
        | Down value -> {Depth=depth; Horizontal=horizontal; Aim=aim+value}
        | Forward value -> {Depth=depth+(aim*value); Horizontal=horizontal+value; Aim=aim}
        | _ -> failwith "impossible"

    let solutionAot2Part2 inputs =
        let lastSubmarineState = 
            inputs 
            |> List.map parseCommand
            |> List.fold applyCommandToSubmarinePart2 baseSubmarine 
        let {Depth=depth; Horizontal=horizontal} = lastSubmarineState
        horizontal*depth