type exp_t = (* ee *)
  | Prop
  | Set
  | Sort 
  | Ident of string
  | Op of string
  | Period
  | Unit (* () *)
  | Blank
  | Blank_num of int
  | Qed
  | Meta of string
  | Sep of string
  | Left of string
  | Vert of string 
  | Right of string
  | String of string
  | Char of char
  | Num of int
  | Decimal of string
  | PeriodPat of exp_t
  | Inferred of exp_t
  | Par of exp_t
  | Seq of exp_t list
  | Exp_list of exp_t list * (exp_t * exp_t list * exp_t) option
  | App of exp_t list
  | Typed of exp_t * exp_t
  | Assign of exp_t * exp_t
  | Binder of string
  | Binderhead of exp_t * (exp_t list)
  | Match of exp_t
  | Lethead of exp_t * exp_t
  | Ifhead of exp_t * exp_t
  | Head of exp_t * exp_t
  | Matchhead of (exp_t list) * exp_t
  | Matchfull of (exp_t list* (exp_t list * exp_t) list)
  | Unicode_unknown of string
  | Unknown
[@@deriving show]

let is_prop e = match e with Prop -> true | _ -> false;;
let mk_prop() = Prop;;
let is_set e = match e with Set -> true | _ -> false;;
let mk_set() = Set;;
let is_sort e = match e with Sort -> true | _ -> false;;
let mk_sort() = Sort;;
let is_ident e = match e with Ident _ -> true | _ -> false;;
let mk_ident s = Ident s;;
let get_ident_name e = match e with Ident s -> s | _ -> failwith "not an Ident";;
let is_op e = match e with Op _ -> true | _ -> false;;
let mk_op s = Op s;;
let get_op e = match e with Op s -> s | _ -> failwith "not an Op";;
let is_period e = match e with Period -> true | _ -> false;;
let mk_period() = Period;;
let is_period_pat e = match e with PeriodPat _ -> true | _ -> false;;
let get_period_pat e = match e with PeriodPat e -> e | _ -> failwith "not a PeriodPat";;
let is_blank e = match e with Blank -> true | _ -> false;;
let mk_blank () = Blank;;
let is_blank_num e = match e with Blank_num _ -> true | _ -> false;;
let mk_blank_num i = Blank_num i;;
let get_blank_num e = match e with Blank_num i -> i | _ -> failwith "not a Blank_num";;
let is_qed e = match e with Qed -> true | _ -> false;;
let mk_qed() = Qed;;
let is_meta e = match e with Meta _ -> true | _ -> false;;
let mk_meta s = Meta s;;
let get_meta e = match e with Meta s -> s | _ -> failwith "not a Meta";;
let is_string e = match e with String _ -> true | _ -> false;;
let get_string e = match e with String s -> s | _ -> failwith "not a String";;
let mk_string s = String s;;
let is_char e = match e with Char _ -> true | _ -> false;;
let mk_char c = Char c;;
let get_char e = match e with Char c -> c | _ -> failwith "not a Char";;
let is_num e = match e with Num _ -> true | _ -> false;;
let mk_num n = Num n;;
let get_num e = match e with Num n -> n | _ -> failwith "not a Num";;
let is_decimal e = match e with Decimal _ -> true | _ -> false;;
let mk_decimal s = Decimal s;;
let get_decimal e = match e with Decimal s -> s | _ -> failwith "not a Decimal";;
let is_exp_list e = match e with Exp_list _ -> true | _ -> false;;
let mk_exp_list (e1,e2) = Exp_list (e1,e2);;
let get_exp_list e = match e with Exp_list (e1,e2) -> (e1,e2) | _ ->failwith "not an Exp_list";;
let is_app e = match e with App _ -> true | _ -> false;;
let get_app e = match e with App es -> es | _ -> failwith "not an App";;
let mk_app es = App es;;
let is_match e = match e with Match _ -> true | _ -> false;;
let get_match e = match e with Match es -> es | _ -> failwith "not a Match";;
let mk_match es = Match es;;
let is_typed e = match e with Typed _ -> true | _ -> false;;
let get_typed e = match e with Typed (e1,e2) -> (e1,e2) | _ -> failwith "not a Typed";;
let mk_typed (e1,e2) = Typed (e1,e2);;
let is_sep e = match e with Sep _ -> true | _ -> false;;
let get_sep e = match e with Sep s -> s | _ -> failwith "not a Sep";;
let mk_sep s = Sep s;;
let is_left e = match e with Left _ -> true | _ -> false;;
let get_left e = match e with Left s -> s | _ -> failwith "not a Left";;
let mk_left s = Left s;;
let is_right e = match e with Right _ -> true | _ -> false;;
let get_right e = match e with Right s -> s | _ -> failwith "not a Right";;
let mk_right s = Right s;;
let is_vert e = match e with Vert _ -> true | _ -> false;;
let get_vert e = match e with Vert s -> s | _ -> failwith "not a Vert";;
let mk_vert s = Vert s;;
let is_par e = match e with Par _ -> true | _ -> false;;
let get_par e = match e with Par s -> s | _ -> failwith "not a Par";;
let mk_par s = Par s;;
let is_assign e = match e with Assign _ -> true | _ -> false;;
let get_assign e = match e with Assign (e1,e2) -> (e1,e2) | _ -> failwith "not a Assign";;
let mk_assign (e1,e2) = Assign (e1,e2);;
let is_lethead e = match e with Lethead _ -> true | _ -> false;;
let get_lethead e = match e with Lethead (e1,e2) -> (e1,e2) | _ -> failwith "not a Lethead";;
let mk_lethead (e1,e2) = Lethead (e1,e2);;
let is_ifhead e = match e with Ifhead _ -> true | _ -> false;;
let get_ifhead e = match e with Ifhead (e1,e2) -> (e1,e2) | _ -> failwith "not an Ifhead";;
let mk_ifhead (e1,e2) = Ifhead (e1,e2);; 
let is_binder e = match e with Binder _ -> true | _ -> false;;
let get_binder e = match e with Binder s -> s | _ -> failwith "not a Bind";;
let mk_binder s = Binder s;;
let is_binderhead e = match e with Binderhead _ -> true | _ -> false;;
let get_binderhead e = match e with Binderhead (e1,e2) -> (e1,e2) | _ -> failwith "not a Binderhead";;
let mk_binderhead (e1,e2) = Binderhead (e1,e2);;
let is_seq e = match e with Seq _ -> true | _ -> false;;
let get_seq e = match e with Seq es -> es | _ -> failwith "not a Seq";;
let mk_seq es = Seq es;;
let is_matchhead e = match e with Matchhead _ -> true | _ -> false;;
let get_matchhead e = match e with Matchhead (e1,e2) -> (e1,e2) | _ -> failwith "not a Matchhead";;
let mk_matchhead (e1,e2) = Matchhead (e1,e2);;
let is_matchfull e = match e with Matchfull _ -> true | _ -> false;;
let get_matchfull e = match e with Matchfull es -> es | _ -> failwith "not a Matchfull";;
let mk_matchfull es = Matchfull es;;
let is_unicode_unknown e = match e with Unicode_unknown _ -> true | _ -> false;;
let get_unicode_unknown e = match e with Unicode_unknown s -> s | _ -> failwith "not a Unicode_unknown";;
let mk_unicode_unknown s = Unicode_unknown s;;
let is_unknown e = match e with Unknown -> true | _ -> false;;
let mk_unknown () = Unknown;;

(* obsolete, replaced by deriving show
let wrap s = "["^s^"]"
           
let lists f xs = String.concat " " (List.map f xs)
let opt f =
  function
    None -> "None"
  | Some e -> "Some "^f e;;
let pr f (e1,e2) = "("^ f e1 ^","^ f e2 ^ ")";;

           
let rec to_string = function
  | Prop -> wrap "Prop"
  | Set -> wrap "Set"
  | Ident s -> wrap ("Ident "^s)
  | Op s -> wrap ("Op "^s)
  | Period -> wrap (".")
  | PeriodPat e -> wrap ("PeriodPat "^to_string e)
  | Blank -> wrap ("_")
  | Blank_num i -> wrap ("_# "^string_of_int i)
  | Qed -> wrap ("QED")
  | String s -> wrap ("String "^s)
  | Char c -> wrap ("Char "^Char.escaped c)
  | Num i -> wrap ("Num "^string_of_int i)
  | Decimal s -> wrap ("Decimal "^s)
  | Exp_list (es,o) -> wrap("Exp_list "^lists to_string es ^ opt (fun (e1,eas,e2) -> to_string e1 ^ lists to_string eas ^ to_string e2) o)
  | App es -> wrap ("App " ^ lists to_string es)
  | Match e -> wrap("Match "^to_string e)
  | Typed (e1,e2) -> wrap("Typed "^to_string e1^ " : "^to_string e2)
  | Sep s -> wrap("Sep "^s)
  | Left s -> wrap("Left "^s)
  | Vert s -> wrap("Vert "^s)
  | Right s -> wrap("Right "^s)
  | Par e -> wrap ("Par "^to_string e)
  | Assign (e1,e2) -> wrap(to_string e1 ^" := "^ to_string e2)
  | Lethead (e1,e2) -> wrap("let "^ to_string e1 ^" := "^ to_string e2 ^" in... ")
  | Ifhead (e1,e2) -> wrap("if " ^to_string e1 ^ " then " ^ to_string e2 ^" else...")
  | Binder s -> wrap ("binder "^s)
  | Binderhead (e,es) -> wrap("Binderhead "^ to_string e ^" "^ lists to_string es)
  | Seq es -> wrap("Seq "^ lists to_string es)
  | Matchhead (es,e) -> wrap("match "^lists to_string es ^" with " ^to_string e ^" in ... ")
  | Matchfull (e,es) -> wrap("matchfull "^lists to_string e^
           lists (fun (e1,e2) -> "("^lists to_string e1^","^to_string e2^")") es)
  | Unicode_unknown s -> wrap("Unicode_unknown "^s)
  | Unknown -> wrap("Unknown");;
 *)

type fixity =
| Left of int
| Right of int
| Fix of int
| List of int 
| Binder
| Accent
| Abbrev ;;


type dec_t =
  | Dec_unknown
  | Retro
  (*  | Context of bool*(exp_t list) *)
  (*  | Post of exp_t * (exp_t list) * exp_t *)
  | Thm of exp_t * (exp_t list) * exp_t * exp_t
  | Def of exp_t * (exp_t list) * (exp_t option) * exp_t
  | Defnote of string * int option * exp_t * (exp_t list) * (exp_t option) * exp_t
  | Hint of exp_t * exp_t list * exp_t list * exp_t * exp_t 
  | Struc of  exp_t * (exp_t list) * (exp_t option) * (((exp_t option)*exp_t) list)
  (*  | Extend of exp_t * (exp_t list) * (((exp_t option)*exp_t) list) *)
  | Ind of exp_t * (exp_t list) * exp_t option * ((exp_t * exp_t list * exp_t option) list)
  | Mut of (exp_t list) * (exp_t list) * ((exp_t * exp_t list * exp_t * ((exp_t * exp_t list * exp_t) list)) list)
  | Error_declaration of string
[@@deriving show];;

type hash_t =
  | Hashprint_con of exp_t list
  | Hashprint_sys of exp_t list
  | Hashprint_dec of exp_t list
  | Hashprint_pac of exp_t list
  | Hashprint_not of exp_t list 
  | Hashcheck of exp_t
  | Hasheval of exp_t
  | Hashconfig of string*exp_t
  | Hash_invalid of string
[@@deriving show]

type progitem_t =
  | Pack of string
  | Import of (string * (exp_t option)) list
  | Notation of (int option * string option * (exp_t list) * exp_t)
  | Dec of dec_t
  | Hash of hash_t
  | Context of bool * (exp_t list)
  | Error of string 
[@@deriving show]              

type program_t = progitem_t list
(*                  
  { pack : string;
    (*    meta : (string*string) list; *)
    import : (string * (exp_t option)) list;
    notation : (int option * string option * (exp_t list) * exp_t) list;
    declaration : dec_t list;
  }
 *)
[@@deriving show]
;;
