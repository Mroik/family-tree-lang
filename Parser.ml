open Combinator

exception Exception

type gender_type = Male | Female | GenderError;;

type probability = Probability of int;;
type transmissible_characteristic =
    | Trans_conditional of string
    | Trans_probability of probability
    | Trans_error
;;

type char_of_hereditary =
    | Hered_conditional of string
    | Hered_probability of int
    | Hered_Error
;;

type characteristic =
    | Char_Error
    | Gender of gender_type
    | Transmissible of string * transmissible_characteristic list
    | Absent_characteristic of string
    | Hereditary of string * char_of_hereditary list
    | Simple of string
;;


(* sex_decl *)
let gender =
    let inner_parser queue =
        let pp = (parse_string "male") #| (parse_string "female") in
        let p = ((parse_string "is ") #~ skip_whitespace) #~> (pp) #<~ (skip_whitespace #~ (parse_char ';')) in
        match run_parser (p) queue with
        | Failure (a, qq) -> Failure (GenderError, qq)
        | Success ("male", qq) -> Success (Male, qq)
        | Success ("female", qq) -> Success (Female, qq)
        | _ -> raise Exception
    in
    Parser (inner_parser)
;;

let sex_decl =
    let inner_parser queue =
        match run_parser (gender) queue with
        | Success (g, qq) -> Success (Gender g, qq)
        | Failure (g, qq) -> Failure (Gender g, qq)
    in
    Parser (inner_parser)
;;


(* transmissible_char *)
let trans_prob =
    let inner_parser queue =
        match run_parser (parse_int_literal) queue with
        | Failure (a, qq) -> Failure (Probability 0, qq)
        | Success (a, qq) ->
            let valu = String.trim a |> int_of_string in
            Success (Probability valu, qq)
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
        | Success (a, qq) -> Success (Trans_conditional a, qq), qq
        | Failure (a, qq) ->
            match run_parser (trans_prob) queue with
            | Failure (a, qq2) -> Failure (Trans_error, qq2), qq2
            | Success (a, qq2) -> Success (Trans_probability a, qq2), qq2
        in
        match run_parser ((skip_whitespace) #~ (parse_char ';') #~ (skip_whitespace)) rest with
        | Failure (a, qq) -> Failure (Trans_error, queue)
        | Success (a, ff) ->
            match ris with
            | Success (a, rr) -> Success (a, ff)
            | Failure (a, vv) -> Failure (a, queue)
    in
    Parser (inner_parser)
;;

let trans_char_rep queue =
    let rec loop acc qq =
        match run_parser (trans_char) qq with
        | Failure (_, qq2) -> List.rev acc, qq2
        | Success (a, qq2) -> loop (a :: acc) qq2
    in
    loop [] queue
;;

let transmissible_char =
    let inner_parser queue =
        let p1 = ((parse_string "can") #~ (skip_whitespace)) #~ (parse_string "transmit") #~ (skip_whitespace) in
        let p2 = (parse_str_literal) #<~ ((skip_whitespace) #~ (parse_char '{') #~ (skip_whitespace)) in
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
            Failure (Char_Error, qq4)
        else
            Success (Transmissible (name, cc_list), qq4)
    in
    Parser (inner_parser)
;;


(* absent_char *)
let absent_char =
    let inner_parser queue =
        let t = (parse_string "doesn't") #~ (skip_whitespace) #~ (parse_string "have") #~ (skip_whitespace) in
        let p = t #~> (parse_str_literal) in
        match run_parser (p) queue with
        | Failure (_, qq) -> Failure (Char_Error, qq)
        | Success (a, qq) -> Success (Absent_characteristic a, qq)
    in
    Parser (inner_parser)
;;


(* hereditary *)
let hered_condi =
    let inner_parser queue =
        let p = ((parse_string "partner") #~ (skip_whitespace)) #~> (parse_str_literal) in
        match run_parser (p) queue with
        | Failure (a, qq) -> Success (Hered_Error, qq)
        | Success (a, qq) -> Success (Hered_conditional a, qq)
    in
    Parser (inner_parser)
;;

let hered_prob =
    let inner_parser queue =
        match run_parser (parse_int_literal) queue with
        | Failure (_, qq) -> Failure (Hered_Error, qq)
        | Success (a, qq) -> Success (Hered_probability (int_of_string a), qq)
    in
    Parser (inner_parser)
;;

let char_of_hereditary =
    let inner_parser queue =
        let p = (skip_whitespace) #~ (parse_char ';') #~ (skip_whitespace) in
        let ris, rr = match run_parser (hered_prob) queue with
        | Success (a, qq) -> a, qq
        | Failure (_, _) ->
            match run_parser (hered_condi) queue with
            | Success (b, qq2) -> b, qq2
            | Failure (b, qq2) -> b, qq2
        in
        match run_parser (p) rr with
        | Failure (a, qq) -> Failure (Hered_Error, queue)
        | Success (a, qq) ->
            if ris = Hered_Error then
                Failure (ris, queue)
            else
                Success (ris, qq)
    in
    Parser (inner_parser)
;;

let char_of_hereditary_rep queue =
    let rec loop acc qq =
        match run_parser (char_of_hereditary) qq with
        | Success (a, qq2) -> loop (a :: acc) qq2
        | Failure (_, _) -> List.rev acc, qq
    in
    loop [] queue
;;

let hereditary =
    let inner_parser queue =
        let p1 = (parse_string "has") #~ (skip_whitespace) #~ (parse_string "hereditary") #~ (skip_whitespace) in
        let p2 = (parse_str_literal) #<~ ((skip_whitespace) #~ (parse_char '{') #~ (skip_whitespace)) in
        let p4 = parse_char '}' in

        let v1, qq1 = match run_parser (p1) queue with
        | Failure (_, qq) -> false, qq
        | Success (a, qq) -> true, qq
        in
        let name, qq2 = match run_parser (p2) qq1 with
        | Failure (_, qq) -> "", qq
        | Success (a, qq) -> a, qq
        in
        let chars, qq3 = char_of_hereditary_rep qq2 in
        let v4, qq4 = match run_parser (p4) qq3 with
        | Failure (_, qq) -> false, qq
        | Success (_, qq) -> true, qq
        in
        if not v1 || name = "" || List.length chars = 0 || not v4 then
            Failure (Char_Error, queue)
        else
            Success (Hereditary (name, chars), qq4)
    in
    Parser (inner_parser)
;;
