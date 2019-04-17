open Bigint
open Exptree
open Lexer
open Parser

let table : ((string * Exptree.value) list) ref = ref [];;

let process_definition definition =
  let x = (Exptree.extension !table definition)  in
    (let rec remove_occurance z tab = match (List.mem_assoc (fst z) tab) with
      true -> remove_occurance z (List.remove_assoc (fst z) tab)
      | false -> tab in
    let rec remove_redundant tab1 tab2 = match tab2 with
      [] -> tab1 | z :: zs -> remove_redundant (remove_occurance z tab1) zs
        in (x @ (remove_redundant !table x) ))

let process_result tree = (Exptree.eval tree !table)

let rec print_table (table : ((string * Exptree.value) list)) =
  match table with
    [] -> ()
  | x :: xs -> print_string("\n"^(fst x)^" :"); print_val(snd x); print_table xs

and print_val vl = match vl with
  | Exptree.NumVal(x) -> print_string(" Integer : ");(print_int(x));
  | Exptree.BoolVal(b) -> if b then print_string(" Bool : True ") else print_string(" Bool : False ")
  | Exptree.TupVal(n, tl) -> print_string(" Tuple (");
    (let rec printup tl = match tl with [] -> print_char(')') | x::[] -> print_val(x); print_char ')'| x::xs -> print_val(x); print_char(','); printup(xs) in printup(tl))
  | Exptree.Func(_, st, _) -> print_string(" Function ")
  | Exptree.RecFunc(_, nm, _, _) -> print_string(" Recursive Function ")
  | Exptree.WatchVal -> print_string("Table"); print_table(!table)
  | NullVal -> ()
  | ExitVal -> exit(0)
;;

let _ =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      try
          Printf.printf "»»» "; flush stdout;
          print_val (process_result (Parser.exp_parser Lexer.read (lexbuf)));
          print_newline()
      with
      | (Read_Definition) -> (table := process_definition (Parser.def_parser Lexer.read (lexbuf)))
      | _ as e -> raise e
    with
    | Exptree.Not_Found(s) -> print_string("\r\r\rVariable "^s^" not in table (at time of abstraction).\n")
    | (Lexer.Bad_Char s) -> print_string("\r\r\rIllegal Character : "); print_char(s); print_string("\n")
    | Stack_overflow -> print_string("\r\r\rStack Overflow\n")
    | _  -> print_string("\r\r\rError\n")
  done
