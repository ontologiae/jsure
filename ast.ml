(* AST *)

open Conduit;;

type info = {
  i_start : int;
  i_end : int;
  i_extra : bool
};;

let version = 8;;



type position = { x : int ; y : int}

type type_possible =
        | Array
        | Object
        | Int
        | Float
        | Bool
        | Regexp
        | Null_type
        | Undefined_type
        | OnSaisPas
        | Autre of string

type 'a pointer = Null | Pointer of 'a ref;;

let new_pointer x = Pointer (ref x);;

let ( !^ ) = function
 | Null -> invalid_arg "Attempt to dereference the null pointer"
 | Pointer r -> !r;;
(*val ( !^ ) : 'a pointer -> 'a = <fun>*)


let ( ^:= ) p v =
 match p with
 | Null -> invalid_arg "Attempt to assign the null pointer"
 | Pointer r -> r := v;;
(*val ( ^:= ) : 'a pointer -> 'a -> unit = <fun>*)


type info_instruction = {
        mutable types_possibles : type_possible list;
        mutable chemin_parcouru : position list;
        mutable is_potentiellement_undefined : bool;(*= List.exists Undefined types_possibles*)
        mutable is_certainement_undefined    : bool;(*= List.length types_possibles = 0 && List.hd types_possibles = Undefined*)
}


let fais_pas_chier = {
        types_possibles = [];
        chemin_parcouru = [];
        is_potentiellement_undefined = true;
        is_certainement_undefined = true;

}

(*let dictionnaire = ExtHashtbl.Hashtbl.create 128;;*)




(*
type object_info = {
        TODO : prototype (de façon récursive)
}*)

type program = source_element list
and source_element =
| St of int * int * instr
| FunDecl of int * int * func
and block = instr list
and instr =
| Expr of expr * position * info_instruction pointer (*Rajouter la position d'origine, en position option : va permettre de noter l'origine de l'expression, si c'est une variable*)
                          (*Rajouter les types possibles, avec le type type_possible. *)
                          (*Trouver un moyen de tracer l'origine de la variable, avec un type permettant de stocker le chemin de celle-ci*)
                          (*Et faire une hashtable sur la position, et la construire, sur TOUTES les positions d'instructions!*)
                          (*Penser au fait qu'il va falloir tagguer les fonctions ayant le même nom dans des prototypes différents avec Proto___fonction*)
                          (*Par défaut le type possible contient Undefined --> Les erreurs seront affichées là ou il ne sera pas éliminé*)
                          (*Elimination du code mort*)
                          (*Tout cela sera construit durant la depending pass*)
| If of expr * instr * instr option * position * info_instruction pointer 
| Do of instr * expr * position * info_instruction pointer
| While of expr * instr * position * info_instruction pointer
| For of instr option * instr option * instr option * instr * position * info_instruction pointer
| Continue of label option * position * info_instruction pointer
| Break of label option * position * info_instruction pointer
| Return of expr option * position * info_instruction pointer
| With of expr * instr * position * info_instruction pointer
| Labeled of label * instr * position * info_instruction pointer
| Switch of expr * (case_clause list * instr) list * position * info_instruction pointer
| Throw of expr * position * info_instruction pointer
| Try of instr * (arg * instr) option * instr option * position * info_instruction pointer
| Variable of variable_declaration list * position * info_instruction pointer
| Block of instr list * position * info_instruction pointer
| ForIn of lhs_or_var * expr * instr * position * info_instruction pointer
| Nop
and lhs_or_var =
| LHS of expr
| Vars of variable_declaration list
and variable_declaration = name * expr option * info_instruction pointer 
and case_clause = Default | Case of expr
and func = name option * name list * source_element list
and extra = int * int * extra_tag
and extra_tag =
| DanglingComma
and expr =
| Assign of expr * assignment_operator * expr
| Sq of expr list
| Function of  func * position 
| L of litteral
| U of unop * expr
| B of binop * expr * expr
| V of name
| Object of (property_name * expr) list
| Array of array_litteral
| Apply of expr * expr list
| Conditional of expr * expr * expr
| This
| Extra of extra
and property_name =
| PN_String of string
| PN_Float of float
| PN_Int of int32
| PN_Empty
and binop =
| B_mul
| B_div
| B_mod
| B_add
| B_sub
| B_le
| B_ge
| B_lt
| B_gt
| B_instanceof
| B_in
| B_equal
| B_lsr
| B_asr
| B_lsl
| B_notequal
| B_physequal
| B_physnotequal
| B_bitand
| B_bitor
| B_bitxor
| B_and
| B_or
| B_bracket
and unop =
| U_bitnot
| U_delete
| U_void
| U_typeof
| U_pre_increment
| U_pre_decrement
| U_post_increment
| U_post_decrement
| U_plus
| U_minus
| U_not
| U_new
and assignment_operator =
| A_eq
| A_mul
| A_div
| A_mod
| A_add
| A_sub
| A_lsl
| A_lsr
| A_asr
| A_and
| A_xor
| A_or
and litteral =
| Float of float
| Int of int32
| String of string
| Regexp of string * string
| Bool of bool
| Null
| Undefined
and array_litteral = expr list
and name = string
and arg = string
and label = string


let info0 = { i_start = 0; i_end = max_int; i_extra = true };;
(*
let apply_in_position pos f =
        match pos with 
        | Position  ( x , y , st)                               -> Position ( x, y, f st)
        |  elem        -> elem

let extract_from_position pos =
        match pos with 
        | Position  ( x , y , st)                               ->  st
        |  elem        -> elem


*)
(*
let rec simplify_ast source =
        match source with
       (* | Position  ( x , y , st) -> 1
        | Expr  ( expr) -> 1
        | If  ( expr , block1 , block2) as if_b -> if_b*)
     (*   | Position  ( x , y , st)                               ->  Position  ( x , y , simplify_ast st)*)
        | Expr (Function ( (name,namelist,source_el),pos),p,infos)       -> Expr (Function ( (name,namelist, (simplify_program source_el)),pos),p,infos )
        | Do  ( st , expr, pos, infos)                                      -> print_endline "Do !!" ; Block ( [st; While(expr,st , pos, infos)],  pos, infos)
        | While  ( expr , st, pos, infos) as while_                         ->  while_
        | For  ( stp  , stc  , staction  , block, pos, infos)               -> print_endline "For !!" ;
                        let pre  = match stp with
                                        | None   -> Nop 
                                        | Some i -> i in
                        let cond = 
                                 match stc with
                                        | None                          -> Nop
                                        | Some   x -> x
                                        | _                             -> failwith "Pas d'expre dans la condition du for" in
                        let action = match staction with 
                                       | None   -> Expr(L(Bool true), pos, infos)
                                       | Some i ->  i in 
                        let blockinstr = match block with
                                        | None ->  l
                                        | Some l -> l in
                        Block [pre;(While(cond,Block (action::blockinstr)))]


    (*    | Continue  ( label (* option *)) -> 1
        | Break  ( label (* option *)) -> 1
        | Return  ( expr (* option *)) -> 1
        | With  ( expr , st) -> 1
        | Labeled  ( label , st) -> 1
        | Switch  ( expr , (case_clause (* list *) , st) (* list *)) -> 1
        | Throw  ( expr) -> 1
        | Try  ( st , (arg , st) (* option *) , st (* option *)) -> 1
        | Variable  ( variable_declaration (* list *)) -> 1
        | Block  ( st (* list *)) -> 1 *)
        | ForIn  ( (Vars exprvars) , expr , st, pos, infos) as foreach      -> foreach
        | _  as instr                                           -> instr


and simplify_source_element s =
        match s with
        | St ( i,j,s) -> St ( i,j,simplify_ast s)
        | _ as reste  -> reste


and simplify_program p = 
        List.map simplify_source_element p 
       

*)


(* iter_over_expr_in_program : info -> (info -> expr -> unit) -> source_element list -> unit*)
let rec iter_over_expr_in_program info f p = List.iter (iter_over_expr_in_source_element info f) p
(*
 * iter_over_expr_in_source_element :
  info -> (info -> expr -> unit) -> source_element -> unit
*)
and extrait_vars lst = 
                        List.map (fun vl -> let nom, expre, infos_instr = vl in (nom,expre)) lst 

and iter_over_expr_in_source_element info f = function
| St(start_pos, end_pos, s) -> iter_over_expr_in_st { info with i_start = start_pos; i_end = end_pos } f s
| FunDecl(_,_,(_,_,sl)) -> iter_over_expr_in_program info f sl
and iter_over_expr_in_sto info f = function
| None -> ()
| Some s -> iter_over_expr_in_st info f s
and iter_over_expr_in_variable_declaration_list info f vl =
  List.iter (fun (_, xo) ->
    match xo with
    | None -> ()
    | Some x -> f info x) vl
(*    iter_over_expr_in_st : info -> (info -> expr -> unit) -> st -> unit*)
and iter_over_expr_in_st info f = function
(*| Position(start_pos, end_pos, s) -> iter_over_expr_in_st { info with i_start = start_pos; i_end = end_pos } f s*)
| Expr (x,pos,infos) -> f info x
| If(x, s1, so,pos,infos) -> f info x; iter_over_expr_in_st info f s1;
  begin
    match so with
    | None -> ()
    | Some s -> iter_over_expr_in_st info f s
  end
| Do(s, x,pos,infos)|While(x,s,pos,infos)|With(x,s,pos,infos) -> iter_over_expr_in_st info f s; f info x
| For(so1, so2, so3, s,pos,infos) ->
    iter_over_expr_in_sto info f so1;
    iter_over_expr_in_sto info f so2;
    iter_over_expr_in_sto info f so3;
    iter_over_expr_in_st info f s
| Return((Some x),pos,infos) -> f info x
| Throw (x,pos,infos) -> f info x
| Continue _|Break _ -> ()
|Return (None,pos,infos) -> ()
| Labeled(_, s,pos,infos) -> iter_over_expr_in_st info f s
| Switch(x, cls,pos,infos) ->
    f info x;
    List.iter
      begin fun (cl, s) ->
        iter_over_expr_in_st info f s;
        List.iter
          begin function
            | Default -> ()
            | Case x -> f info x
          end
          cl
      end
      cls
| Try(s, aso, so,pos,infos) ->
    iter_over_expr_in_st info f s;
    begin
      match aso with
      | None -> ()
      | Some(_, s) -> iter_over_expr_in_st info f s
    end;
    begin
      match so with
      | None -> ()
      | Some s -> iter_over_expr_in_st info f s
    end
| Variable (vl,pos,infos) ->
                    iter_over_expr_in_variable_declaration_list info f (extrait_vars vl)
| Block (sl,pos,infos) -> List.iter (iter_over_expr_in_st info f) sl
| ForIn(l, x, s,pos,infos) ->
    f info x;
    iter_over_expr_in_lhs info f l;
    iter_over_expr_in_st info f s
| Nop -> ()
and iter_over_expr_in_lhs info f = function
| LHS x -> f info x
| Vars vl -> iter_over_expr_in_variable_declaration_list info f (extrait_vars vl)
;;

(*** scribe_property_name *)
let scribe_property_name cd oc = function
| PN_String u -> cd.cd_print oc "%S" u
| PN_Float f -> cd.cd_print oc "%f" f
| PN_Int x -> cd.cd_print oc "%ld" x
| PN_Empty -> cd.cd_print oc "*empty*"
;;
(*
 *
 *
 * val iter_over_expr_in_program :
  info -> (info -> expr -> unit) -> source_element list -> unit
val iter_over_expr_in_source_element :
  info -> (info -> expr -> unit) -> source_element -> unit
val iter_over_expr_in_sto :
  info -> (info -> expr -> unit) -> st option -> unit
val iter_over_expr_in_variable_declaration_list :
  info -> (info -> expr -> unit) -> variable_declaration list -> unit
val iter_over_expr_in_st : info -> (info -> expr -> unit) -> st -> unit
val iter_over_expr_in_lhs :
  info -> (info -> expr -> unit) -> lhs_or_var -> unit
val scribe_property_name : 'a Conduit.conduit -> 'a -> property_name -> unit 
*)
