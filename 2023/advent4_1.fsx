open System
open System.IO

let scoreBoard = 0 :: (1 |> Seq.unfold (fun state -> Some (state, state * 2)) |> Seq.take 1000 |> Seq.toList)
let score count = scoreBoard[count]
let xx = 1
let yy = score xx

let trim (str:string) = str.Trim()
let isNotEmpty (str:string) = str.Length > 0
let toIntArray =
    Seq.map trim
    >> Seq.filter isNotEmpty
    >> Seq.map int
    >> Seq.toArray

let out =
    File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample4.txt")
    |> Seq.map (fun line ->
        let split = line.Substring(5).Split(":")
        let split' = split[1].Split("|")
        printfn $"{split[0]} - {split'[0]} - {split'[1]}"
        {| cardNumber = (int split[0])
           winningNumbers = (split'[0].Split(" ") |> toIntArray)
           playedNumbers =  (split'[1].Split(" ") |> toIntArray) |}
        )
    |> Seq.map (fun record ->
        let countOfWinningCards =
            record.playedNumbers
            |> Seq.filter (fun pp -> Array.contains pp record.winningNumbers)
            |> Seq.length
        score countOfWinningCards
        )
    |> Seq.sum

printfn $"{out}"