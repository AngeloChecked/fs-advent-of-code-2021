namespace AdventOfCode2021

module Utils = 
    let read fileName =
        seq(System.IO.File.ReadLines(fileName))


