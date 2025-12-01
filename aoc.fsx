open System.Text.RegularExpressions
// open System.Math

module Day1 =
    type Direction =
        | L
        | R

    type Instruction = { direction: Direction; turns: int }

    let getNumFromStr (input: string) : int =
        let m = Regex.Match(input, @"\d+")
        if m.Success then m.Value |> int else failwith "No int"


    let getPart1: int =
        let input = System.IO.File.ReadAllLines("./day1/input.txt") |> Array.toList
        // let input = [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"; "L201" ]

        let instructions: List<Instruction> =
            input
            |> List.map (fun x ->
                match x with
                | ins when ins.StartsWith("L") ->
                    { direction = L
                      turns = getNumFromStr ins }
                | ins when ins.StartsWith("R") ->
                    { direction = R
                      turns = getNumFromStr ins }
                | _ -> failwith "Bruh what")

        let rotateLeft (n: int) (by: int) : int = snd (System.Math.DivRem(n - by, 100))

        let rotateRight (n: int) (by: int) : int = snd (System.Math.DivRem(n + by, 100))

        snd (
            List.fold
                (fun acc ins ->
                    match ins.direction with
                    | L ->
                        let rotation = rotateLeft (fst acc) ins.turns
                        (rotation, if rotation = 0 then snd acc + 1 else snd acc)
                    | R ->
                        let rotation = rotateRight (fst acc) ins.turns
                        (rotation, if rotation = 0 then snd acc + 1 else snd acc))
                (50, 0)
                instructions
        )

    // ----------------------------------------------------------------------------------------------

    let getPart2: int =
        // let input = System.IO.File.ReadAllLines("./day1/input.txt") |> Array.toList
        let input =
            [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"; "L201" ]

        let instructions: List<Instruction> =
            input
            |> List.map (fun x ->
                match x with
                | ins when ins.StartsWith("L") ->
                    { direction = L
                      turns = getNumFromStr ins }
                | ins when ins.StartsWith("R") ->
                    { direction = R
                      turns = getNumFromStr ins }
                | _ -> failwith "Bruh what")

        let rotateLeft (n: int) (by: int) : int = snd (System.Math.DivRem(n - by, 100))
        // 100 - System.Math.Abs(rem)
        // snd (System.Math.DivRem(n - by, 100))

        let rotateRight (n: int) (by: int) : int =
            // let res = (n + by)
            // if res >= 100 then res - 100 else res
            snd (System.Math.DivRem(n + by, 100))

        // let rem = snd (System.Math.DivRem(n + by, 100))
        // 100 - System.Math.Abs(rem)


        snd (
            List.fold
                (fun acc ins ->
                    printf "%A\n" acc
                    match ins.direction with
                    | L ->
                        let rotation = rotateLeft (fst acc) ins.turns
                        let rotate = 100 - System.Math.Abs(rotation)
                        // printf "Rotated left %A\n" rotation
                        (rotation, (if rotation = 0 then snd acc + 1 else snd acc))
                    | R ->
                        let rotation = rotateRight (fst acc) ins.turns
                        let rotate = 100 - System.Math.Abs(rotation)
                        // printf "Rotated right %A\n" rotation

                        (rotation, (if rotation = 0 then snd acc + 1 else snd acc)))
                (50, 0)
                instructions
        )
