open Combinator

exception Exception

type gender_type = Male | Female | GenderError;;
type probability = Probability of int;;
type characteristic = Transmissible of string * (string option * probability option) list;;


(* sex_decl *)
let gender =
    let inner_parser queue =
        let p = (parse_string "male") #| (parse_string "female") in
        run_parser (p) queue 
    in
    Parser (inner_parser)
;;

let sex_decl =
    let inner_parser queue =
        let p = ((parse_string "is ") #~ skip_whitespace) #~> (gender) #<~ (skip_whitespace #~ (parse_char ';')) in
        match run_parser (p) queue with
        | Failure (a, qq) -> Failure (GenderError, qq)
        | Success ("male", qq) -> Success (Male, qq)
        | Success ("female", qq) -> Success (Female, qq)
        | _ -> raise Exception
    in
    Parser (inner_parser)
;;


(* transmissible_char *)
let trans_prob =
    let inner_parser queue =
        match run_parser (parse_int_literal) queue with
        | Failure (a, qq) -> Failure (Probability 0, qq)
        | Success (a, qq) -> Success (Probability (int_of_string a), qq)
    in
    Parser (inner_parser)
;;

let trans_cond =
    let inner_parser queue =
        let p = ((parse_string "partner") #~ skip_whitespace) #~> parse_str_literal in
        run_parser (p) queue
    in
    Parser (inner_parser)
;;

let trans_char =
    let inner_parser queue =
        let ris, rest = match run_parser (trans_cond) queue with
        | Success (a, qq) -> Success ((Some a, None), qq), qq
        | Failure (a, qq) ->
            match run_parser (trans_prob) queue with
            | Failure (a, qq2) -> Failure ((None, Some a) ,qq2), qq2
            | Success (a, qq2) -> Success ((None, Some a) ,qq2), qq2
        in
        match run_parser ((skip_whitespace) #~ (parse_char ';')) rest with
        | Failure (a, qq) -> Failure ((None, None), qq)
        | _ -> ris
    in
    Parser (inner_parser)
;;

let trans_char_rep queue =
    let rec loop acc qq =
        let v, acc2, qq1 = match run_parser (trans_char) qq with
        | Failure (_, qq2) -> false, acc, qq2
        | Success (a, qq2) -> true, (a :: acc), qq2
        in
        if not v then
            List.rev acc2, qq1
        else
            loop acc2 qq1
    in
    loop [] queue
;;

let transmissible_char queue =
    let p1 = ((parse_string "can") #~ (skip_whitespace)) #~ (parse_string "transmit") #~ (skip_whitespace) in
    let p2 = (parse_str_literal) #<~ ((skip_whitespace) #~ (parse_char '{')) in
    let p3 = trans_char_rep in
    let p4 = (skip_whitespace) #~ (parse_char '}') in

    let v1, qq1 = match run_parser (p1) queue with
    | Failure (_, qq) -> false, qq
    | Success (a, qq) -> true, qq
    in
    let name, qq2 = match run_parser (p2) qq1 with
    | Failure (_, qq) -> "", qq
    | Success (a, qq) -> a, qq
    in
    let cc_list, qq3 = p3 qq2 in
    let qq4 = match run_parser (p4) qq3 with
    | Failure (_, qq) -> qq
    | Success (_, qq) -> qq
    in
    if (not v1) || (name = "") || (cc_list = []) then
        None, qq4
    else
        Some (
            Transmissible (name, cc_list)
        ), qq4
;;
