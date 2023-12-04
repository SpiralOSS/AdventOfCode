open System
open System.IO

// ----------------------------------------------------------------
type WinningCardTracker() =
    let mutable (cardCounts:int list) = [0]
    
    static member private grow (cardCounts:int list) (count:int) : int list =
        cardCounts @ List.init (Math.Clamp(count - cardCounts.Length + 1, 0, Int32.MaxValue) : int) (fun _ -> 0)

    member self.addWinningCards (count:int) =
        cardCounts <-
            WinningCardTracker.grow cardCounts count
            |> Seq.mapi (fun indx num ->
                if (indx < count)
                then num + 1
                else num
                )
            |> Seq.toList
    
    member self.pop () =
        let (head, tail) =
            match cardCounts with
            | [] -> failwith "nope"
            | [x] -> (x, [])
            | head :: tail -> (head, tail)
        cardCounts <- tail
        head
    
    member self.getCardCounts () = cardCounts

// ----------------------------------------------------------------
let trim (str:string) = str.Trim()
let isNotEmpty (str:string) = str.Length > 0
let toIntArray =
    Seq.map trim
    >> Seq.filter isNotEmpty
    >> Seq.map int
    >> Seq.toArray

let out =
    let wct = WinningCardTracker ()
    File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample4.txt")
    |> Seq.map (fun line ->
        let split = line.Substring(5).Split(":")
        let split' = split[1].Split("|")
        {| cardNumber = (int split[0])
           winningNumbers = (split'[0].Split(" ") |> toIntArray)
           playedNumbers =  (split'[1].Split(" ") |> toIntArray) |}
        )
    |> Seq.map (fun record ->
        let bonusScratchCards = wct.pop ()
        let winCount =
            record.playedNumbers
            |> Seq.filter (fun pp -> Array.contains pp record.winningNumbers)
            |> Seq.length
        
        if winCount > 0
        then [ 0..bonusScratchCards ] |> Seq.iter (fun _ -> wct.addWinningCards winCount)
        else wct.addWinningCards 0
        
        bonusScratchCards + 1
        )

printfn $"OUT = {out |> Seq.sum}"
(*
#r "nuget:XUnit"
open Xunit
[<Fact>]
let ``Test WinningCardTracker.addWinningCards Empty`` () =
    let win = WinningCardTracker()
    win.addWinningCards 4
    let actual = win.getCardCounts ()
    let expect = [1;1;1;1;0]
    Assert.Equal<Collections.Generic.IEnumerable<int>>(expect, actual)
``Test WinningCardTracker.addWinningCards Empty`` ()

[<Fact>]
let ``Test WinningCardTracker.addWinningCards Subset`` () =
    let win = WinningCardTracker()
    win.addWinningCards 4
    win.addWinningCards 2
    let actual = win.getCardCounts ()
    let expect = [2;2;1;1;0]
    Assert.Equal<Collections.Generic.IEnumerable<int>>(expect, actual)
``Test WinningCardTracker.addWinningCards Subset`` ()

[<Fact>]
let ``Test WinningCardTracker.addWinningCards Superset`` () =
    let win = WinningCardTracker()
    win.addWinningCards 3
    win.addWinningCards 5
    let actual = win.getCardCounts ()
    let expect = [2;2;2;1;1;0]
    Assert.Equal<Collections.Generic.IEnumerable<int>>(expect, actual)
``Test WinningCardTracker.addWinningCards Superset`` ()

[<Fact>]
let ``Test WinningCardTracker.pop`` () =
    let win = WinningCardTracker()
    win.addWinningCards 4
    win.addWinningCards 4
    Assert.Equal(2, win.pop ())
    let actual = win.getCardCounts ()
    let expect = [2;2;2;0]
    Assert.Equal<Collections.Generic.IEnumerable<int>>(expect, actual)
``Test WinningCardTracker.pop`` ()
*)