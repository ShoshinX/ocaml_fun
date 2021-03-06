
open Extensions;;
(* Day 1*)

let explode s = List.init (String.length s) (String.get s)
let implode s_list = String.init (List.length s_list) (fun i -> List.nth s_list i)

let rec q1_solve input sum =
    match input with
    | [] -> sum
    | '(' :: tl -> q1_solve tl (sum + 1)
    | ')' :: tl -> q1_solve tl (sum - 1)
    | _ -> sum


let rec q1b_solve input sum pos =
    if sum == -1 then pos
    else match input with
    | [] -> pos
    | '(' :: tl -> q1b_solve tl (sum + 1) (pos + 1)
    | ')' :: tl -> q1b_solve tl (sum - 1) (pos + 1)
    | _ -> pos

(* Day 2 *)

let d2_solve input =
    let dim_split = String.split_on_char '\n' input
    in
    let mul_splt eqn = String.split_on_char 'x' eqn
    in
    let process_eqn eqn =
        match eqn with
        | l::w::h::[] -> 2*l*w + 2*w*h + 2*h*l
        | _ -> 1
    in
    let min n1 n2 = if n1 < n2 then n1 else n2
    in
    let min_face nums =
        match nums with
        | l::w::h::[] -> min (l*w) (min (w*h) (h*l))
        | _ -> 0
    in
    let rec process_eqns eqns =
        match eqns with
        | [] -> 0
        | hd::tl -> let nums = List.map int_of_string (mul_splt hd) 
                    in 
                    (process_eqn nums) + min_face nums + process_eqns tl
    in process_eqns dim_split

let d2b_solve input = 
    let dim_split = String.split_on_char '\n' input
    in
    let mul_splt eqn = String.split_on_char 'x' eqn
    in
    let min n1 n2 = if n1 < n2 then n1 else n2
    in
    let min_face_perim nums =
        match nums with
        | l::w::h::[] -> min (2*l+2*w) (min (2*w+2*h) (2*h+2*l))
        | _ -> 0
    in
    let calc_vol nums =
        match nums with
        | l::w::h::[] -> l*w*h
        | _ -> 0
    in
    let rec process_eqns eqns =
        match eqns with
        | [] -> 0
        | hd::tl -> let nums = List.map int_of_string (mul_splt hd)
                    in
                    (min_face_perim nums) + calc_vol nums + process_eqns tl
    in process_eqns dim_split


(* day 3 *)
module IntPairs =
struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
    match compare x0 x1 with
        0 -> compare y0 y1
        | c -> c
end

module PairsSet = Set.Make(IntPairs)

let d3a_solve input x1 y1 =
    let set = PairsSet.(empty |> add (x1,y1))
    in
    let rec traverse set steps curr_x curr_y = 
        match steps with
        | [] -> set
        | '^'::tl -> traverse (PairsSet.add (curr_x, curr_y) set) tl curr_x (curr_y+1)
        | '>'::tl -> traverse (PairsSet.add (curr_x, curr_y) set) tl (curr_x+1) curr_y
        | 'v'::tl -> traverse (PairsSet.add (curr_x, curr_y) set) tl curr_x (curr_y-1)
        | '<'::tl -> traverse (PairsSet.add (curr_x, curr_y) set) tl (curr_x-1) curr_y
        | _ -> set
    in
    traverse set (explode input) x1 y1 |> PairsSet.elements |> List.length

let d3b_solve input x1 y1 =
    let set = PairsSet.(empty |> add(x1,y1))
    in
    let rec traverse_santa set steps santa_x santa_y robo_x robo_y =
        match steps with
        | [] -> set
        | '^'::tl -> traverse_robo (PairsSet.add (santa_x, santa_y) set) tl santa_x (santa_y+1) robo_x robo_y
        | '>'::tl -> traverse_robo (PairsSet.add (santa_x, santa_y) set) tl (santa_x+1) santa_y robo_x robo_y
        | 'v'::tl -> traverse_robo (PairsSet.add (santa_x, santa_y) set) tl santa_x (santa_y-1) robo_x robo_y
        | '<'::tl -> traverse_robo (PairsSet.add (santa_x, santa_y) set) tl (santa_x-1) santa_y robo_x robo_y
        | _ -> set
    and traverse_robo set steps santa_x santa_y robo_x robo_y =
        match steps with
        | [] -> set
        | '^'::tl -> traverse_santa (PairsSet.add (robo_x, robo_y) set) tl santa_x santa_y robo_x (robo_y+1)
        | '>'::tl -> traverse_santa (PairsSet.add (robo_x, robo_y) set) tl santa_x santa_y (robo_x+1) robo_y
        | 'v'::tl -> traverse_santa (PairsSet.add (robo_x, robo_y) set) tl santa_x santa_y robo_x (robo_y-1)
        | '<'::tl -> traverse_santa (PairsSet.add (robo_x, robo_y) set) tl santa_x santa_y (robo_x-1) robo_y
        | _ -> set
    in
    traverse_santa set (explode input) x1 y1 x1 y1 |> PairsSet.elements |> List.length

(* day 4 *)

let d4a_solve input zeroes= 
    let rec loop y =
        let generate_md5 = String.concat "" [input; string_of_int y]
        in
        if (Digest.string generate_md5 |> Digest.to_hex |> String.sub) 0 (String.length zeroes) |> String.equal zeroes then string_of_int y else loop (y+1)
    in
    loop 0


(* day 5 *)

let d5a_solve input = 
    let string_split = String.split_on_char '\n' input 
    in
    let rec is_nice prev_c num_vowels is_twice s =
        let is_twice' = fun x -> if x == prev_c then true else is_twice
        in
        match s with
        | 'a'::'b'::_ -> false
        | 'c'::'d'::_ -> false
        | 'p'::'q'::_ -> false
        | 'x'::'y'::_ -> false
        | 'a'::tl -> is_nice 'a' (num_vowels+1) (is_twice' 'a') tl
        | 'e'::tl -> is_nice 'e' (num_vowels+1) (is_twice' 'e') tl
        | 'i'::tl -> is_nice 'i' (num_vowels+1) (is_twice' 'i') tl
        | 'o'::tl -> is_nice 'o' (num_vowels+1) (is_twice' 'o') tl
        | 'u'::tl -> is_nice 'u' (num_vowels+1) (is_twice' 'u') tl
        | hd::tl -> is_nice hd num_vowels (is_twice' hd) tl
        | [] -> if is_twice == true && num_vowels > 2 then true else false
    in
    List.map (fun x -> explode x |> is_nice '_' 0 false) string_split |> List.filter ((==) true) |> List.length |> string_of_int


let d5b_solve input =
    let string_split = String.split_on_char '\n' input
    in
    let has_repeated s =
        let rec scan = function
            | [] | [_] | [_;_] -> false
            | h :: (_ :: (h' :: _) as t) ->
                    if h = h' then true
                    else scan t
        in
        scan s
    in
    let has_pair_twice s =
        let rec find p = function
            | [] | [_] -> false
            | h :: (h' :: _ as t) ->
                    if (h, h') = p then true
                    else find p t
        in
        let rec scan = function
            | [] | _ :: [] -> false
            | x when List.length x < 4 -> false
            | h :: (h' :: t' as t) ->
                    if find (h,h') t' then true
                    else scan t
        in
        scan s
    in
    List.map (fun x -> (has_repeated (explode x)) && (has_pair_twice (explode x))) string_split |> List.filter ((==) true) |> List.length |> string_of_int


let d6a_solve s =
    let lines = String.split_on_char '\n' s
    in
    let arr = Array.make (1000*1000) 0
    in
    let processLine arr s =
        let ls = String.split_on_char ' ' s
        in
        let coordStrToInt s = 
            String.split_on_char ',' s 
            |> (fun x ->
                    match x with 
                    | (f::s::[]) -> (int_of_string f, int_of_string s)
                    | _ -> (0,0)
            )
        in
        let turnOn  arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    arr.(x) <- 1
                done
            done;
            arr
        in
        let turnOff arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    arr.(x) <- 0
                done
            done;
            arr
        in
        let toggle  arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    if arr.(x) == 0
                    then arr.(x) <- 1
                    else arr.(x) <- 0
                done
            done;
            arr
        in
        match ls with
        | "toggle"::coord1::"through"::coord2::_      -> toggle arr (coordStrToInt coord1) (coordStrToInt coord2)
        | "turn"::"on"::coord1::"through"::coord2::_  -> turnOn arr (coordStrToInt coord1) (coordStrToInt coord2)
        | "turn"::"off"::coord1::"through"::coord2::_ -> turnOff arr (coordStrToInt coord1) (coordStrToInt coord2)
        | _ -> arr
    in
    let rec processLines arr lines =
        match lines with
        | [] -> arr
        | line::tl -> processLines (processLine arr line) tl
    in
    processLines arr lines |> Array.fold_left (+) 0 |> string_of_int


let d6b_solve s =
    let lines = String.split_on_char '\n' s
    in
    let arr = Array.make (1000*1000) 0
    in
    let processLine arr s =
        let ls = String.split_on_char ' ' s
        in
        let coordStrToInt s = 
            String.split_on_char ',' s 
            |> (fun x ->
                    match x with 
                    | (f::s::[]) -> (int_of_string f, int_of_string s)
                    | _ -> (0,0)
            )
        in
        let turnOn  arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    arr.(x) <- arr.(x) + 1
                done
            done;
            arr
        in
        let turnOff arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    if arr.(x) == 0 
                    then arr.(x) <- arr.(x)
                    else arr.(x) <- arr.(x) - 1
                done
            done;
            arr
        in
        let toggle  arr (x1,y1) (x2,y2) =
            for i = x1 to x2 do
                for j = y1 to y2 do
                    let x = j*1000 + i
                    in
                    arr.(x) <- arr.(x) + 2
                done
            done;
            arr
        in
        match ls with
        | "toggle"::coord1::"through"::coord2::_      -> toggle arr (coordStrToInt coord1) (coordStrToInt coord2)
        | "turn"::"on"::coord1::"through"::coord2::_  -> turnOn arr (coordStrToInt coord1) (coordStrToInt coord2)
        | "turn"::"off"::coord1::"through"::coord2::_ -> turnOff arr (coordStrToInt coord1) (coordStrToInt coord2)
        | _ -> arr
    in
    let rec processLines arr lines =
        match lines with
        | [] -> arr
        | line::tl -> processLines (processLine arr line) tl
    in
    processLines arr lines |> Array.fold_left (+) 0 |> string_of_int


type signal =
    | Wire  of string
    | Value of int
type expr = 
    | Equal     of signal
    | And       of signal * signal
    | Lshift    of signal * signal
    | Not       of signal
    | Or        of signal * signal
    | Rshift    of signal * signal

module SS = Map.Make(String)
let d7a_solve s =
    let lines = String.split_on_char '\n' s
    in
    let map = SS.empty
    in
    let map_expr map expr=
        let eq = String.split_on_char ' ' expr
        in
        let signal s = try Value(int_of_string s) with _ -> Wire(s)
        in
        match eq with
        | a::"->"::b::_                 -> SS.add b (Equal (signal a)) map
        | a::"AND"::b::"->"::c::_       -> SS.add c (And (signal a, signal b)) map
        | a::"LSHIFT"::b::"->"::c::_    -> SS.add c (Lshift (signal a, signal b)) map
        | "NOT"::a::"->"::b::_          -> SS.add b (Not (signal a)) map
        | a::"OR"::b::"->"::c::_        -> SS.add c (Or (signal a, signal b)) map
        | a::"RSHIFT"::b::"->"::c::_    -> SS.add c (Rshift (signal a, signal b)) map
        | _                             -> map
    in
    let rec map_exprs exprs map =
        match exprs with
        | [] -> map
        | hd::tl -> map_exprs tl (map_expr map hd)
    in
    let solve signals map =
        let signals = ref signals
        in
        let rec solve_rec w =
            try SS.find w !signals
            with _ ->
                let max_v = 65535
                in
                let signal = function Value(v) -> v | Wire(w) -> solve_rec w
                in
                let activate w s = signals := SS.add w s !signals; s
                in
                let wire = try SS.find w map with _ -> failwith ("bad wiring: " ^ w)
                in
                match wire with
                | Equal(s) ->  signal s |> activate w
                | And (s1, s2) -> (signal s1) land (signal s2) |> activate w
                | Lshift (s1, s2) -> (lsl) (signal s1) (signal s2) |> activate w
                | Not s -> max_v - (signal s) |> activate w
                | Or (s1, s2) -> (signal s1) lor (signal s2) |> activate w
                | Rshift (s1, s2) -> (lsr) (signal s1) (signal s2) |> activate w
        in
        map |> SS.iter (fun k _ -> solve_rec k |> ignore);
        !signals
    in
    solve SS.empty (map_exprs lines map
                (*|> SS.add "b" (Equal(Value(3176)))(* for 7b*)*)
    ) 
    |> SS.find "a" 
    |> string_of_int

let to_hex = Printf.printf "%x"
let from_hex s = Scanf.sscanf s "%x" (fun v -> v)

let d8a_solve channel =
    let hex_to_c chars = chars |> implode |> from_hex |> char_of_int
    in
    let rec decode dst src =
        match src with
        | [] -> dst
        | '"' :: src' -> decode dst src'
        | '\\' :: src' ->
                (match src' with
                | c :: src'' when c = '"' || c = '\\' -> decode (c::dst) src''
                | 'x' :: c1 :: c2 :: src'' -> decode (([c1;c2] |> hex_to_c) :: dst) src''
                | _ -> failwith "invalidInput"
                )
        | c :: src' -> decode (c::dst) src'
    in
    let rec process (count_code, count_str) =
        try
        (input_line channel) |> (fun s -> (String.length s |> (+) count_code, decode [] (explode s) |> implode |> String.length |> (+) count_str)) |> process
        with
        End_of_file -> (count_code, count_str)
    in
    process (0,0) |> (fun (a,b) -> string_of_int (a-b))


let d8b_solve channel =
    let rec encode dst src =
        match src with
        | [] -> dst
        | c :: src' ->
                if c = '"' || c = '\\' then encode (c:: '\\' :: dst) src'
                else encode (c::dst) src'
    in
    let rec process (count_code, count_str) =
        try
        (input_line channel) |> (fun s -> (String.length s |> (+) count_code, encode [] (explode s) |> implode |> Printf.sprintf "\"%s\"" |> String.length |> (+) count_str)) |> process
        with
        End_of_file -> (count_code, count_str)
    in
    process (0,0) |> (fun (a,b) -> string_of_int (b-a))

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;
let d9a_solve input_string = 
    (*construct graph*)
    (*traverse to find shortest hamilton path*)
    let lines = String.split_on_char '\n' input_string
    in
    let parse s = Scanf.sscanf s "%s to %s = %d" (fun from dest dist -> (from,dest,dist))
    in
    let rec construct_routes lines routes =
        let add_route from dest dist routes = 
            let destinations = try StringMap.find from routes with Not_found -> []
            in
            StringMap.add from ((dest,dist)::destinations) routes
        in
        match lines with
        | line::tl -> parse line |> (fun (from, dest, dist) -> add_route from dest dist routes |> add_route dest from dist) |> construct_routes tl
        | [] -> routes
    in
    let routes = construct_routes lines StringMap.empty
    in
    let determine kind routes = 
        let rec travel visited from =
            let destinations = 
                routes |> StringMap.find from |> List.filter (fun (city,_) -> not (StringSet.mem city visited)) |> List.map (fun (city, dist) -> dist + travel (StringSet.add from visited) city)
            in
            match destinations with
            | [] -> 0
            | _ -> destinations |> kind
        in
        let keys m = StringMap.fold (fun k _ acc -> k :: acc) m []
        in
        routes |> keys |> List.map (travel StringSet.empty) |> kind
    in
    let min items =
        let rec search items =
        match items with
        | [] -> failwith "min requires a non-empty list"
        | [x] -> x
        | x :: tail -> min x @@ search tail
        in
        search items
    in
    determine (min) routes |> string_of_int


let d9b_solve input_string = 
    (*construct graph*)
    (*traverse to find shortest hamilton path*)
    let lines = String.split_on_char '\n' input_string
    in
    let parse s = Scanf.sscanf s "%s to %s = %d" (fun from dest dist -> (from,dest,dist))
    in
    let rec construct_routes lines routes =
        let add_route from dest dist routes = 
            let destinations = try StringMap.find from routes with Not_found -> []
            in
            StringMap.add from ((dest,dist)::destinations) routes
        in
        match lines with
        | line::tl -> parse line |> (fun (from, dest, dist) -> add_route from dest dist routes |> add_route dest from dist) |> construct_routes tl
        | [] -> routes
    in
    let routes = construct_routes lines StringMap.empty
    in
    let determine kind routes = 
        let rec travel visited from =
            let destinations = 
                routes |> StringMap.find from |> List.filter (fun (city,_) -> not (StringSet.mem city visited)) |> List.map (fun (city, dist) -> dist + travel (StringSet.add from visited) city)
            in
            match destinations with
            | [] -> 0
            | _ -> destinations |> kind
        in
        let keys m = StringMap.fold (fun k _ acc -> k :: acc) m []
        in
        routes |> keys |> List.map (travel StringSet.empty) |> kind
    in
    let max items =
        let rec search = function
        | [] -> failwith "a non-empty list is required"
        | [x] -> x
        | x :: tail -> max x (search tail)
        in
        search items
    in
    determine (max) routes |> string_of_int

let d10a_solve input_string repetitions= 
    let rec parse c n result line = 
        match line with
        | [] ->  ((Printf.sprintf "%c%d" c n |> explode) @ result) |> List.rev
        | c'::tl when c = c' -> parse c' (n+1) result tl
        | c'::tl -> parse c' 1 ((Printf.sprintf "%c%d" c n |> explode) @ result) tl
    in
    let rec repeat times input = if times == 0 then input else repeat (times - 1) (parse (List.hd input) 1 [] (List.tl input))
    in
    repeat repetitions (explode input_string) |> implode |> String.length |> string_of_int

let d11a_solve input_string =
    let rec has_straight pwd_ints = 
        match pwd_ints with
        | [] | [_] | [_;_] -> false
        | c :: (c' :: c'' :: _ as t) -> if (c + 1) = c' && (c' + 1) = c'' then true else has_straight t
    in
    let rec has_no_forbidden pwd_ints = 
        match pwd_ints with 
        | hd :: tl -> if hd = int_of_char 'i' || hd = int_of_char 'o' || hd = int_of_char 'l' then false else has_no_forbidden tl
        | [] -> true
    in
    let has_2_pair pwd_ints = 
        let rec find p = function
            | [] | [_] -> None
            | h :: (h' :: _ as t) ->
                    match p with
                    | None -> if h = h' then Some(h) else find p t
                    | Some(x) -> if h = h' && h != x then Some(x) else find p t
        in 
        match find (find None pwd_ints) pwd_ints with
        | None -> false
        | Some(_) -> true
    in
    let next pwd_ints =
        let first = int_of_char 'a' and last = int_of_char 'z' in
        let rec inc pwd_ints = 
            match pwd_ints with
            | [] -> []
            | h::t -> 
                    if h != last then (h+1)::t
                    else (first) :: (inc t) 
        in
        pwd_ints |> List.rev |> inc |> List.rev
    in
    let rec next_valid pwd_ints = 
        let next_pwd = next pwd_ints in
        let ok = [has_no_forbidden;has_straight;has_2_pair] |> List.for_all (fun p -> p next_pwd) in
        if ok then next_pwd 
        else next_valid next_pwd
    in
    input_string |> explode |> List.map (fun x -> int_of_char x) |> next_valid |> List.map (fun x -> char_of_int x) |> implode

(*---I did not do this one because it felt like a chore to lex and parse json---*)
let rec sum_of_numbers sum = function
  | Json.Object(elements) -> sum +. (elements |> List.fold_left (fun acc (_, e) -> acc +. sum_of_numbers 0. e) 0.)
  | Json.Array(elements)  -> sum +. (elements |> List.fold_left (fun acc e -> acc +. sum_of_numbers 0. e) 0.)
  | Json.Number(num)      -> sum +. num
  | _ -> 0.
;;

let rec sum_of_non_red_numbers sum = function
  | Json.Object(elements) ->
    if elements |> List.exists (fun (_, v) -> match v with Json.String(str) when str = "red" -> true | _ -> false) then 0.
    else sum +. (elements |> List.fold_left (fun acc (_, e) -> acc +. sum_of_non_red_numbers 0. e) 0.)
  | Json.Array(elements)  -> sum +. (elements |> List.fold_left (fun acc e -> acc +. sum_of_non_red_numbers 0. e) 0.)
  | Json.Number(num)      -> sum +. num
  | _ -> 0.
;;

let d12_part1 () =
  File.open_in "day12.input" (fun ch ->
    Stream.of_chars ch
    |> Json.parse_stream
    |> sum_of_numbers 0.
    |> Printf.printf "part 1: the sum is %g\n"
  )
;;
d12_part1 ();;

let d12_part2 () =
  File.open_in "day12.input" (fun ch ->
    Stream.of_chars ch
    |> Json.parse_stream
    |> sum_of_non_red_numbers 0.
    |> Printf.printf "part 1: the sum is %g\n"
  )
;;
d12_part2 ();;

let add_rule who next_to change rules =
    let personal_rules = try rules |> StringMap.find who with _ -> StringMap.empty
    in rules |> StringMap.add who (personal_rules |> StringMap.add next_to change)
;;
let parse line rules =
    Scanf.sscanf line "%s would %s %d happiness units by sitting next to %[^\\.]" (fun who gain_or_lose units next_to ->
        let change = match gain_or_lose with
        | "gain" -> units
        | "lose" -> -units
        | _ -> failwith (Printf.sprintf "Invalid change: %s" gain_or_lose)
        in
        add_rule who next_to change rules
    )
;;
let eval rules seating =
    let lookup who next_to =
        rules |> StringMap.find who |> StringMap.find next_to
    in
    let rec eval first sum = function
        | [] -> 0
        | [last] -> sum + (lookup last first) + (lookup first last)
        | current :: (next :: _ as rest) -> eval first (sum + (lookup current next) + (lookup next current)) rest
    in
    match seating with
    | [] | [_] -> 0
    | first :: _ -> eval first 0 seating
;;
let permutations seating = 
    let p = Array.of_list seating in
    let n = Array.length p in

    let next_perm () =
        let i = let rec aux i =
            if (i<0) || (p.(i) < p.(i+1)) then i
            else aux (i - 1) in aux (n - 2) in
        let rec aux j k = if j < k then
            let t = p.(j) in
            p.(j) <- p.(k);
            p.(k) <- t;
            aux (j + 1) (k - 1)
        else () in aux (i + 1) (n - 1);
        if (i < 0) then false else
            let j = let rec aux j =
                if p.(j) > p.(i) then j
                else aux (j + 1) in aux (i + 1) in
            let t = p.(i) in
            p.(i) <- p.(j);
            p.(j) <- t;
            true
    in
    let next _ =
        if next_perm () then Some(Array.to_list p)
        else None
    in
    Stream.from next
;;
let optimize seating rules = 
    permutations seating
    |> Stream.map (eval rules)
    |> Stream.max
;;

let part1 () =
  File.open_in "day13.input" (fun ch ->
    let rules =
      Stream.of_lines ch
      |> Stream.fold parse StringMap.empty
    in
    let seating =
      rules |> StringMap.bindings |> List.map (fun (k, _) -> k)
    in
    let total_change = optimize seating rules in
    Printf.printf "part1: total change: %d\n" total_change
  )
;;
part1 ();;

let part2 () =
  File.open_in "day13.input" (fun ch ->
    let rules =
      Stream.of_lines ch
      |> Stream.fold parse StringMap.empty
    in
    let guests =
      rules |> StringMap.bindings |> List.map (fun (k, _) -> k)
    in
    let rules' = guests |> List.fold_left (fun r g -> r |> add_rule "me" g 0 |> add_rule g "me" 0) rules in
    let seating = "me" :: guests in
    let total_change = optimize seating rules' in
    Printf.printf "part2: total change: %d\n" total_change
  )
;;
part2 ();;




let d8a_input_channel = open_in "day8_input.txt"
let d8b_input_channel = open_in "day8_input.txt"
let solutions = 
    [(string_of_int (q1_solve (explode Inputs.q1_input) 0))
    ;(string_of_int (q1b_solve (explode "()())") 0 0))
    ;(string_of_int (d2_solve Inputs.d2_input))
    ;(string_of_int (d2b_solve Inputs.d2_input))
    ;(string_of_int (d3a_solve Inputs.d3_input 0 0))
    ;(string_of_int (d3b_solve Inputs.d3_input 0 0))
    (*
    ;d4a_solve "yzbqklnj" "00000"
    ;d4a_solve "yzbqklnj" "000000"
    *)
    ;d5a_solve Inputs.d5_input
    ;d5b_solve Inputs.d5_input
    ;d6a_solve Inputs.d6_input
    ;d6b_solve Inputs.d6_input
    ;d7a_solve Inputs.d7_input
    ;d8a_solve d8a_input_channel
    ;d8b_solve d8b_input_channel
    ;d9a_solve Inputs.d9_input
    ;d9b_solve Inputs.d9_input
    (*
    ;d10a_solve "1113222113" 40
    ;d10a_solve "1113222113" 50
    *)
    ;d11a_solve "cqjxjnds"
    ;d11a_solve "cqjxjnds" |> d11a_solve
    ]

let () =
    let rec print_solutions solutions =
        match solutions with
        | [] -> print_endline "DONE!"
        | hd::tl -> print_endline hd; print_solutions tl
    in
    print_solutions solutions
