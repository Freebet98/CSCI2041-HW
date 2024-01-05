(** This generates the duck verses into a string array, it calls itself recursively
    This takes an int n, which gets pattern matched againsted
    This takes an int i, which holds the original value of n
*)
let rec generate_duck_helper (n: int) (i : int): string list =
    match n with
    | 0 -> 
        ["Mama duck went swimming one day\n
        Over the hills and far away\n
        The mama duck said, \"Quack, quack, quack, quack\"\n
        And all " ^ (string_of_int i) ^ " little ducks came back"]
    | 1 -> 
        ["1 little duck went swimming one day\n
        Over the hills and far away\n
        The mama duck said, \"Quack, quack, quack, quack\"\n
        And then no more little ducks came back"] @ generate_duck_helper (n - 1) i
    | 2 -> 
        ["2 little ducks went swimming one day\n
        Over the hills and far away\n
        The mama duck said, \"Quack, quack, quack, quack\"\n
        And only 1 little duck came back"] @ generate_duck_helper (n - 1) i
    | _ -> 
        [(string_of_int n) ^ " little ducks went swimming one day\n 
        Over the hills and far away\n 
        The mama duck said, \"Quack, quack, quack, quack\"\n 
        And only " ^ (string_of_int (n-1)) ^ " little ducks came back"] @ generate_duck_helper (n - 1) i
    
(** This will check if n is less than or equal to 0, if not, it will call another
    function to generate the verse and returns a string 
    Takes an int: n *)
let generate_duck_verse(n: int) =
    if n <= 0 then failwith "n should be greater than 0"
    else
        let duck_lst = generate_duck_helper n n in String.concat "\n" duck_lst

(** This will print the duck verse out
    Takes an int: n *)
let print_duck_verse(n: int) =
    print_endline (generate_duck_verse n)