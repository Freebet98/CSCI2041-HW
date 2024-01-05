type exercise = 
| Int of int
| Mult of exercise * int
| Plus of exercise * int

let exercise_one = (Mult (Plus (Plus (Mult (Int 3, 15), 0), 2), 1))
let exercise_two = Mult (Plus (Mult (Plus (Int 0, 4), 1), 0), 6)
let exercise_three = (Int 64)

let rec solution (prob : exercise) :  int =
        match prob with
        | Int i -> i 
        | Mult (e1, i) -> (solution e1) * i 
        | Plus (e1, i) -> (solution e1) + i 

let rec string_of_exercise (prob : exercise) : string =
        match prob with
        | Int i -> string_of_int i 
        | Mult (e1, i) -> string_of_exercise e1 ^ " * " ^ string_of_int i ^ " -> ... "
        | Plus (e1, i) -> string_of_exercise e1 ^ " + " ^ string_of_int i ^ " -> ... "

let rec string_of_solution (prob : exercise) : string =
        match prob with
        | Int i -> string_of_int i 
        | Mult (e1, i) -> string_of_solution e1 ^ " * " ^ string_of_int i ^ " -> " ^ string_of_int(solution(Mult(e1,i)))
        | Plus (e1, i) -> string_of_solution e1 ^ " + " ^ string_of_int i ^ " -> " ^ string_of_int(solution(Plus(e1,i)))

let rec from_random  (nums : int list) (signs : bool list) : exercise =
        match nums with
        | [] -> Int 0
        | hd :: tl -> 
                match signs with
                |[] -> Int hd
                | head::tail -> 
                        if head = true 
                                then Mult(from_random tl tail, hd)
                        else
                                Plus(from_random tl tail, hd)


let rec filterNonTrivial (prob : exercise) : exercise =
        match prob with
        | Int i -> Int i 
        | Mult (e1, i) -> 
                if i != 1 then
                        Mult(filterNonTrivial e1, i)
                else filterNonTrivial e1
        | Plus (e1, i) -> 
                if i != 0 then
                        Plus(filterNonTrivial e1, i)
                else filterNonTrivial e1

let rec signs_lst_comp prob =
        match prob with
                | Int i  -> []
                | Mult (e1, i) -> true :: signs_lst_comp e1
                | Plus (e1, i) -> false :: signs_lst_comp e1

let rec nums_lst_comp prob = 
        match prob with
                | Int i -> i :: []
                | Mult (e1, i) -> i :: nums_lst_comp e1
                | Plus (e1, i) -> i :: nums_lst_comp e1

let find lst i = 
        let rec find lst i n =
                match lst with
                | [] -> None
                | hd :: tl -> if (hd = i) then Some(n) else find tl i (n+1)
        in find lst i 0

let rec split lst i j = 
        if i > j then
                []
        else 
                (List.nth lst i) :: split lst (i + 1) j

let get j = 
        match j with
        | Some(c) -> c 
        | None -> 0

let splitOnMultZero (prob : exercise) : (exercise * exercise) option =
        let signs_lst = signs_lst_comp prob in 
        let nums_lst = nums_lst_comp prob in 
        let find_val = find nums_lst 0 in
        let numsl = split nums_lst 0 (get(find_val)) in
        let numsr = split nums_lst (get(find_val)+1) (List.length nums_lst-1) in
        let signsl = split signs_lst 0 (get(find_val)) in
        let signsr = split signs_lst (get(find_val)+1) (List.length signs_lst-1) in

        if find_val = None then None
        else 
                match nums_lst with
                |[] -> None
                |hd :: tl -> 
                        if hd = 0 then None
                        else Some((from_random numsr signsr), (from_random numsl signsl))

let rec keepSplitting (prob : exercise) : exercise list =
        match prob with
        | Int i -> Int i :: []
        | Mult (e1, i) -> Int i :: keepSplitting e1
        | Plus (e1, i) -> Int i :: keepSplitting e1

let rec printExercise (probs : exercise list) : unit =
  match probs with
  | h :: tl -> print_string ((string_of_exercise h)^"\n"^(string_of_solution h)^"\n\n"); printExercise tl
  | [] -> ()

let rec genProblems (nums : int list) (signs : bool list) : unit =
  (printExercise (keepSplitting (filterNonTrivial (from_random nums signs))))
