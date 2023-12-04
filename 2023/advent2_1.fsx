open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
#r "nuget:FsToolkit.ErrorHandling"
open FsToolkit.ErrorHandling

type Object =
    | Red
    | Green
    | Blue
    | None

let (|Draw|) (draw:string) =
    let rmatch = Regex.Match (draw, "^\s*(?<Count>\d+?)\s*(?<Type>\w+)\s*$")
    let object =
        match rmatch.Groups["Type"].Value with
        | "green" -> Green
        | "red" -> Red
        | "blue" -> Blue
        | _ -> None
    let count = int rmatch.Groups["Count"].Value
    Draw (object, count)

// GAME
// ROUNDS (DRAW list)
// DRAWS (Object * count)

let getRounds (gameString:string) =
    gameString.Split ";"
    |> Seq.map (fun round ->
        let draws = Dictionary<_,_>()
        round.Split ","
        |> Seq.iter (fun (Draw (object, count)) ->
            draws[object] <- draws.GetValueOrDefault(object, 0) + count 
            )
        let draws' = draws |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> Seq.toList
        draws'
        )

type Game =
    { id: int
      rounds: ((Object*int) list) seq }

let validateRound (round:(Object*int) list) =
    round
    |> Seq.map (fun draw ->
        match draw with
        | (Red, x) when x <= 12 -> true
        | (Green, x) when x <= 13 -> true
        | (Blue, x) when x <= 14 -> true
        | _ -> false
        )
    |> Seq.forall id

let out =
    File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample2.txt")
    |> Seq.map (fun line -> line.Substring(5).Split ":")
    |> Seq.map (fun parts ->
        { id = int parts[0]
          rounds = getRounds parts[1] }
        )
    |> Seq.filter (fun game -> game.rounds |> Seq.forall validateRound)
    |> Seq.sumBy (fun game -> game.id)

printfn $"{out}"
