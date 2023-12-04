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
    let mins = Dictionary<_,_> ()
    gameString.Split ";"
    |> Seq.iter (fun round ->
        round.Split ","
        |> Seq.iter (fun (Draw (object, count)) ->
            if mins.GetValueOrDefault(object, 0) < count
            then mins[object] <- count
            else ()
            )
        )
    mins |> Seq.map (fun kv -> (kv.Key, kv.Value))

type Game =
    { id: int
      mins: (Object*int) seq }

let out =
    File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample2.txt")
    |> Seq.map (fun line -> line.Substring(5).Split ":")
    |> Seq.map (fun parts ->
        { id = int parts[0]
          mins = getRounds parts[1] }
        )
    |> Seq.map (fun game ->
        (1, (game.mins |> Seq.map snd)) ||> Seq.fold (fun acc num -> acc * num)
        )
    |> Seq.sum

printfn $"{out}"
