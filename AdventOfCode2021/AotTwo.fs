namespace AdventOfCode2021

module DayTwo =
    type Submarine = { Depth: int; Horizontal: int } 
    let baseSubmarine = { Depth=0; Horizontal=0 } 

    type Command = Up of int 
                 | Down of int
                 | Forward of int 

    let parseComand (line:string): Command = 
        let splitted = line.Split(' ') |> Array.toList
        match splitted with
        | ["up"; value] -> Up (int value)
        | ["down"; value] -> Down (int value)
        | ["forward"; value] -> Forward (int value)
        | _ -> failwith "impossible"

    let applyCommandToSubmarine {Depth=depth; Horizontal=horizontal} (command:Command) :Submarine = 
        match command with
        | Up value -> {Depth=depth-value; Horizontal=horizontal}
        | Down value -> {Depth=depth+value; Horizontal=horizontal}
        | Forward value -> {Depth=depth; Horizontal=horizontal+value}
        | _ -> failwith "impossible"

    let solutionAot2 inputs =
        let lastSubmarineState = 
            inputs 
            |> List.map parseComand
            |> List.fold applyCommandToSubmarine baseSubmarine 
        let {Depth=depth; Horizontal=horizontal} = lastSubmarineState
        horizontal*depth
