(* a few lines from HOL Light lib.ml *)

(*let compare = Stdlib.compare;;*)

let report s =
  Format.print_string s; Format.print_newline();;

let warn cond s =
  if cond then report ("Warning: "^s) else ();;

let curry f x y = f(x,y);;

let uncurry f(x,y) = f x y;;

let rec assocd a l d =
  match l with
    [] -> d
  | (x,y)::t -> if compare x a = 0 then y else assocd a t d;;

let rec rev_assocd a l d =
  match l with
    [] -> d
  | (x,y)::t -> if compare y a = 0 then x else rev_assocd a t d;;

let rec zip l1 l2 =
  match (l1,l2) with
        ([],[]) -> []
      | (h1::t1,h2::t2) -> (h1,h2)::(zip t1 t2)
      | _ -> failwith "zip";;

let rec unzip =
  function [] -> [],[]
         | ((a,b)::rest) -> let alist,blist = unzip rest in
                            (a::alist,b::blist);;

let rec partition p l =
  match l with
    [] -> [],l
  | h::t -> let yes,no = partition p t in
            if p(h) then (if yes == t then l,[] else h::yes,no)
            else (if no == t then [],l else yes,h::no);;

let rec itlist2 f l1 l2 b =
  match (l1,l2) with
    ([],[]) -> b
  | (h1::t1,h2::t2) -> f h1 h2 (itlist2 f t1 t2 b)
  | _ -> failwith "itlist2";;

let rec butlast l =
  match l with
    [_] -> []
  | (h::t) -> h::(butlast t)
  | [] -> failwith "butlast";;

let rec last l =
  match l with
    [x] -> x
  | (_::t) -> last t
  | [] -> failwith "last";;

let rec chop_list n l =
  if n = 0 then [],l else
  try let m,l' = chop_list (n-1) (List.tl l) in (List.hd l)::m,l'
  with Failure _ -> failwith "chop_list";;

let _ = chop_list 3 [5;6;7;8;9;10;11;12];; (* gives [5;6;7], [8;9;10;11;12] *)

(* end HOL Light *)

let pad k x ls =  (* output has length k. Adjust length at head.  *)
  let r = List.length ls - k in 
  if (r >= 0) then snd(chop_list r ls)
    else (List.init (-r) (fun _ -> x) @ ls);;

let _ = pad 1 'x' ['t';'a';'b';'c';'d'];;  (* ['d'] *)

let _ = pad 3 'x' ['t'] (* ['x'; 'x'; 't'] *)

let safetail =
  function
  | [] -> []
  | _ :: tl -> tl

let take k ls =  if k <= 0 then [] else fst(chop_list k ls) (* retain head part *)

let rec cutat p =  (* head of list satisfies p *)
  function
  | [] -> failwith "cutat not found "
  | t :: ts as ls -> if p t then ls else cutat p ts 

let list_opt =
  function
  | None -> []
  | Some t -> [t]

let opt_list = 
  function 
  | [] -> None
  | t :: _ -> Some t

let (-|) f g x = f(g(x))

let pair x y = (x,y)

let discard _ = ();;

let id x = x

let omap f =
function
| Some x -> Some (f x)
| None -> None

let diag2 f (x,y) = (f x,f y)

let diag3 f (x,y,z) = (f x,f y,f z)

let fun2 (f,g) (x,y) = (f x,g y)

let fun3 (f1,f2,f3) (x1,x2,x3) = (f1 x1,f2 x2,f3 x3)

let fun4 (f1,f2,f3,f4) (x1,x2,x3,x4) = (f1 x1,f2 x2,f3 x3,f4 x4)

let fun5 (f1,f2,f3,f4,f5) (x1,x2,x3,x4,x5) = (f1 x1,f2 x2,f3 x3,f4 x4,f5 x5)

let join2 (x,y) = (x @ y)

let join3 (x,y,z) = (x @ y @ z)

let join4 (x1,x2,x3,x4) = (x1 @ x2 @ x3 @ x4)

let join5 (x1,x2,x3,x4,x5) = (x1 @ x2 @ x3 @ x4 @ x5)

let jdiag2 f = join2 -| diag2 f

let jdiag3 f = join3 -| diag3 f

let jfun2 f = join2 -| fun2 f

let jfun3 f = join3 -| fun3 f

let jfun4 f = join4 -| fun4 f

let jfun5 f = join5 -| fun5 f








