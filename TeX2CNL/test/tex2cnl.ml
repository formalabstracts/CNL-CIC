type token = 
   | Natural of int
   | Numeric of string
   | Eol
   | ControlSeq of string
   | ControlChar of string
   | Arg of int
   | LParen
   | RParen
   | LBrack
   | RBrack
   | LBrace
   | RBrace
   | LDisplay
   | RDisplay
   | Dollar
   | Sub
   | FormatEol 
   | FormatCol 
   | Tok of string
   | Symbol of string
   | Eof
   | NotImplemented;;

let stream = ref [];;
let popstream() =
 let a = List.hd !stream in
 let _ = stream := List.tl !stream in
 a;;


let outstream = ref [];;

let pushout a =
 let _ = outstream := a :: (!outstream) in ();;

let process1 state = 
 if !stream = [] then ()
 else 
  let a = popstream() in
  match a with
  | ControlSeq s -> process1CS s
  | ControlChar s -> process1CC s
  | Dollar -> ()
  | Sub -> processSub
  | FormatEol -> processFormatEol
  | FormatCol -> processFormatCol
  | _ -> pushout a;;


;;


List.hd [1;2;3];;
List.tl [1;2;3];;
