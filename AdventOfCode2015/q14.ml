(*open Extensions;;
*)
let parse line = 
    Scanf.sscanf line "%s can fly %d km/s for %d seconds, but then must rest for %d seconds."
        (fun reindeer km moving_sec stationary_sec -> 
            (reindeer, km, moving_sec, stationary_sec))
;;

(*
    given:  reindeer, km/s, moving_sec, stationary_sec
            total_time
    solution outline:
        |--------------------|---     ---|---------|-------------|
        |moving + stationary |    ...    |x% moving|y% stationary|
        |--------------------|---     ---|---------|-------------|
    chunk_time = (moving_sec + stationary_sec)
    number of chunks = total_time / chunk_time
    number of sec left in the last chunk = total_time % chunk_time
    moving sec in last chunk = min(moving_sec, number of sec left in the last chunk)
    max_distance = number_of_chunks * km * moving_sec + moving_sec_in_last_chunk * km
 *)
let eval_max_d total_time reindeer = 
    match reindeer with
    | (_, km, moving_sec, stationary_sec) ->
            let chunkTime = moving_sec + stationary_sec in
            let numberOfChunks = total_time / chunkTime in
            let numberOfSecLeftInLastChunk = total_time mod chunkTime in
            let moving_sec_in_last_chunk = min moving_sec numberOfSecLeftInLastChunk in
            numberOfChunks * km * moving_sec + moving_sec_in_last_chunk * km
;;

let test1 = ["Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.";"Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."]
;;

let input =
"Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
Rudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.
Donner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.
Blitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.
Comet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.
Cupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Dancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.
Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds."

type time_left = int
type position = int
type speed = int
type speed_time = int
type rest_time = int

type motionState =
    | Resting of rest_time 
    | Moving of speed * speed_time 

type name = string
type points = int
type reindeerState =
    | Reindeer of name * position * time_left * motionState * points

let getMoveState name reindeerList =
    List.find (fun reindeer -> let (reindeerName, _, _) = reindeer in String.equal name reindeerName) reindeerList
    |> (fun reindeer -> let (_,moveState,_) = reindeer in moveState)

let getRestState name reindeerList =
    List.find (fun reindeer -> let (reindeerName, _, _) = reindeer in String.equal name reindeerName) reindeerList
    |> (fun reindeer -> let (_,_,restState) = reindeer in restState)

exception GetRestStateHasWrongStates;;
exception MoveStateWrong;;
let initialiseSimulation ticks listOfReindeers = 
    let reindeerList = List.map (fun (reindeer,speed,moving_sec, stationary_sec) -> (reindeer, Moving(speed,moving_sec), Resting(stationary_sec))) listOfReindeers  in
    let reindeerInitialStates = List.map (fun (name, moveState, _) -> 
                                            match moveState with
                                                Moving(_, time) -> Reindeer(name, 0, time, moveState, 0)
                                                |_ -> raise MoveStateWrong
                                        ) reindeerList in
    let calculateNextState reindeerState =
        match reindeerState with
        | Reindeer (name, pos, t, state, points) ->
            let moveState = getMoveState name reindeerList in
            let restState = getRestState name reindeerList in
            let obtainTime state = match state with
                | Moving(_, t) -> t
                | Resting(t) -> t
            in
            let t' = t - 1 in
            begin match state with
            | Resting _ -> Reindeer (name, pos, (if t' = 0 then obtainTime moveState else t'), (if t'= 0 then moveState else state), points)
            | Moving (speed, _) -> Reindeer (name, pos + speed, (if t' = 0 then obtainTime restState else t'), (if t'= 0 then restState else state), points)
            end
    in
    let rec simulate ticks reindeerStates =
        if ticks = 0 then reindeerStates 
        else 
            let reindeerNextStates = List.map calculateNextState reindeerStates in
            let max_pos = List.fold_left (fun max_num nextState -> 
                                           match nextState with 
                                                Reindeer(_, pos, _, _, _) ->
                                                    max max_num pos
                                         ) 0 reindeerNextStates 
            in
            let reindeerNextStates' = List.map (function state ->
                                                match state with Reindeer(name, pos, t, state', points) ->
                                                    if pos = max_pos then Reindeer (name, pos, t, state', points + 1)
                                                    else Reindeer(name, pos, t, state', points)
            ) reindeerNextStates
            in
            simulate (ticks - 1) reindeerNextStates'
    in
    simulate ticks reindeerInitialStates

let max_points = 
    input
    |> String.split_on_char '\n'
    |> List.map (parse)
    |> initialiseSimulation 2503
    
    |> List.fold_left (fun max_point reindeer -> match reindeer with Reindeer(_, _, _, _, points) -> max max_point points) 0
    |> string_of_int
    

let () = 
    let total_time = 2503 in
    input
    |> String.split_on_char '\n'
    |> List.map (parse)
    |> List.map (eval_max_d total_time)
    |> (fun xs -> match xs with (hd::tl) -> List.fold_left (max) (hd) (tl)
                                | [] -> 0)
    |> string_of_int
    |> print_endline;print_endline max_points; print_endline "Done";

