(* Check *)

open Ast;;
open Source;;

module H = BatHashtbl;;









let fonctions = H.create 128;;
H.add fonctions "f" "djqdjqlkjdsqjlkd";;
H.remove fonctions "f";;


type 'a pointer = NullPointer | Pointer of 'a ref;;

let ( !^ ) = function
 | NullPointer -> invalid_arg "Attempt to dereference the null pointer"
 | Pointer r -> !r;;

let ( ^:= ) p v =
 match p with
 | NullPointer -> invalid_arg "Attempt to assign the null pointer"
 | Pointer r -> r := v;;



let new_pointer x = Pointer (ref x);;





type func_call = {
                        pere : string;
                        fun_call : string;
};;


let call_list_init : func_call list = [] ;;

let dyn_call_list = new_pointer call_list_init;;


let push el = let ll = !^dyn_call_list in dyn_call_list ^:= (BatList.cons el ll);;

let addFunc f a = push { 
                                pere = f; (*pid*)
                                fun_call = a (*id*)
                        } ;;


let reset () = 1;;


let h2l h = BatList.of_enum (BatHashtbl.enum h);;
(*
let rec difference  m1 m2 = match m1 with
  | []          -> []
  | t::q when List.mem t m2 -> difference q (BatList.remove m2 t)
  | t::q        -> t::(difference  q m2);;

let rec intersect m = function
  | []                     -> []
  | t::q when BatList.mem t m -> t :: (intersect (BatList.remove m t) q)
  | t :: q                 -> intersect m q;;
*)
type arbre =
        | Feuille of string
        | Noeud   of string * arbre list;;



 let trouve_et_enleve h cle =
                let h2l h = BatList.of_enum (BatHashtbl.enum h) in
                let prnt  = List.iter (fun c -> let a,b = c in print_endline (a^" -> "^b.fun_call)) in
                let elem = H.find_all h cle in
                let _    = H.remove h cle in
                let _    = prnt (h2l h) in
                elem ;;






let fusionne l = let groupes = BatList.group (fun a -> fun b -> if a = b then 0 else 1) l in
        let combineOne l =
                let couples = List.map (
                        fun liste_meme -> match liste_meme with
                        | Noeud (a, ll) -> Noeud(a,[]), ll
                        | Feuille a -> Feuille a, []
                        ) l
                in
                let tete,_ = List.hd couples in
                let _,souslistes = BatList.split couples in
                match tete with
                | Noeud(a,[] ) -> Noeud(a,BatList.concat souslistes)
                | Feuille a as f -> f in
         List.map combineOne groupes;;


let hash_verif_recursiv_arbre =  H.create 1024 ;;


let rec construit_arbre hpid  noeudss feuilles n nodee =
                (* je cherche dans la H, toutes les fonctions déclarées ayant le nom de fun_call*)
                let _          = H.add hash_verif_recursiv_arbre nodee (string_of_int n) in
                let  node     = match BatList.length (H.find_all hash_verif_recursiv_arbre nodee ) > 30 with
                                        | true  -> {pere = "RECURSION" ; fun_call = "RECURSION"}
                                        | false -> nodee in
                let sous_arbre =  BatList.unique (
                                List.map (construit_arbre hpid noeudss feuilles (n+1)) 
                                         (BatList.flatten (BatList.map (fun elem -> BatList.filter (fun e -> elem.fun_call = e.pere) (noeudss@feuilles)) (H.find_all hpid node.pere)))
                                ) in
                       (* List.map (construit_arbre hpid noeudss) (BatList.flatten (BatList.map (fun e -> H.find_all hpid e.fun_call)  (H.find_all hpid node.pere) ))  in*)
                match BatList.exists ( fun e -> e.pere = node.pere) noeudss with
                | false ->  Feuille node.pere
                | true  ->  if n < 30 then Noeud (node.pere, BatList.unique (fusionne sous_arbre)) else Feuille (node.pere^"__RECURSION__");;



let arborify arbolist =
         let hpid1 = H.create 1024 in
        let _   = List.iter (fun n -> H.add hpid1 n.pere n) arbolist in

        let arbolistf = BatList.unique  (arbolist@(BatList.unique (BatList.map (fun i -> { pere = i.fun_call ; fun_call = ""} )   (List.filter (fun e -> not (H.mem hpid1 e.fun_call) )  arbolist)))) in
               (*Liste des pères*)
        let hpid = H.create 1024 in
        let _   = List.iter (fun n -> H.add hpid n.pere n) (List.filter (fun n -> not (n.fun_call = "")) arbolistf) in

        (* liste des items *)
        let hid = H.create 1024 in
        let _   = List.iter (fun n -> H.add hid n.fun_call n) arbolistf in

        (* Liste des élément sans racines*)
        let racines = BatList.unique (List.filter (fun n -> not (H.mem hid n.pere) ) arbolistf) in


        let feuilles = BatList.unique (H.find_all hid "") in
        let  noeuds = let _,n = (BatList.split (h2l hpid)) in BatList.unique n in
        fusionne (BatList.unique (List.map (construit_arbre hpid noeuds feuilles 0) racines))




let rec to_string  nbindent n =
        let string_repeat s n = Array.fold_left (^) "" (Array.make n s) in
        let indentation = string_repeat "|   " nbindent  in
        match n with
        | Noeud   (t, q) -> indentation^"├── "^t^"\n"^(String.concat "" (List.map (to_string (nbindent+1)) q))
        | Feuille t      -> indentation^"├── "^t^"\n";;


           


  (** check_source_element ***)
  let rec check_source_element fonction_mere  = function
    | St(start_pos, end_pos, s) ->
        check_statement fonction_mere s
    | FunDecl(start_pos, end_pos, func) ->
          match func with
          | (Some name, _, _) ->
                    print_endline "On est dans une déclaration de fonction !!!";                          
                    check_function  name func 
          | (None, _, _) -> check_function (fonction_mere^".__anonyme__") func

        (** check_function*)
  and check_function fonction_mere (no, al, sl) =
    List.iter (check_source_element fonction_mere) sl
    (** check_statement_option*)
  and check_statement_option fonction_mere = function
    | None -> ()
    | Some s -> check_statement fonction_mere s

    (** check_expr_as_lhs *)
  and check_expr_as_lhs fonction_mere = function
    | V name -> ()
    | B(B_bracket, x1, x2) ->
      check_expr fonction_mere x1;
      check_expr fonction_mere x2;
    | _ -> ()


    (** check_expr*)
  and check_expr  fonction_mere = function
    | Extra(start_pos, end_pos, DanglingComma) -> ()
    | Apply(x, xl) ->
      (*Remplissage callgraph : on est dans quelle fonction, et on appelle quel fonction?*)
                    let nom_appel   = 
                            match x with
                            | V n -> n
                            | Function (i,j, (None, _,_)) -> "anonymous"
                            | B (B_bracket, This, L (String str)) -> "this."^str
                            | B (B_bracket, V receveur, L (String message)) -> receveur^"."^message 
                            | B (B_bracket, Apply (V receveur, [L (String argument)]), L (String methode)) -> receveur^"("^argument^")"^"."^methode
                            | B (B_bracket, Apply (V receveur, [V argument]), L (String methode))          -> receveur^"("^argument^")"^"."^methode
                            | _   -> "__" in
                    print_endline "On est dans une application de fonction !!!";
                    addFunc fonction_mere nom_appel;
                    check_expr fonction_mere x;
                    List.iter (check_expr fonction_mere) xl
    | Assign (B (B_bracket, V receveur, L (String methode)), A_eq, Function(i,j,f) ) -> check_expr (receveur^"."^methode) (Function(i,j,f))
    | Assign(x1, _, x2) ->
      check_expr_as_lhs fonction_mere x1;
      check_expr fonction_mere x2;
    | Sq xl -> List.iter (check_expr fonction_mere) xl
    | L(Regexp(x, o)) -> ()
    | L(Float _) | L(Int _) | L(String _) | L(Bool _) | L Null | L Undefined  -> ()
    | Conditional(x1, x2, x3) ->
        check_expr fonction_mere x1;
        check_expr fonction_mere x2;
        check_expr fonction_mere x3
    | V name -> ()
    | U(_, x) -> check_expr fonction_mere x
    | B(_, x1, x2) ->
      begin
        check_expr fonction_mere x1;
        check_expr fonction_mere x2
      end
    | This -> ()
    | Array xl -> List.iter (check_expr fonction_mere) xl
    | Object pl -> List.iter (fun (_, x) -> check_expr fonction_mere x) pl
    | Function(start_pos, end_pos, func) -> 
                    let nom,vars,code = func in
                    let nom_final = 
                            match nom with
                            | None   ->  fonction_mere^"-->anonyme__"^(string_of_int start_pos)
                            | Some n ->  n in
                    check_function nom_final func



  (** check_statement *)
  and check_statement fonction_mere st =
    match st with
    | Position(start_pos, end_pos, s) ->
        check_statement fonction_mere s
    | Expr x -> check_expr fonction_mere x
    | If(x, s, so) ->
        check_expr fonction_mere x;
        check_statement fonction_mere s;
        check_statement_option fonction_mere so
    | Do(s, x) ->
        check_statement fonction_mere s;
        check_expr fonction_mere x
    | While(x, s) ->
        check_expr fonction_mere x;
        check_statement fonction_mere s
    | For(so1, so2, so3, s) ->
        check_statement_option fonction_mere so1;
        check_statement_option fonction_mere so2;
        check_statement_option fonction_mere so3;
        check_statement fonction_mere s
    | ForIn(lv, x, st) ->
        check_lhs_or_var fonction_mere lv;
        check_expr fonction_mere x;
        check_statement fonction_mere st
    | Throw x -> check_expr fonction_mere x
    | Continue _ -> () (* XXX Check labels *)
    | Break _ -> () (* XXX Check labels *)
    | Labeled _ -> ()
    | Return None -> ()
    | Return(Some x) -> check_expr fonction_mere x
    | With(x, s) ->
        check_expr fonction_mere x;
        check_statement fonction_mere s
    | Variable vl -> List.iter (check_variable_declaration fonction_mere) vl
    | Block sl ->
      List.iter (check_statement fonction_mere) sl
    | Nop -> ()
    | Try(s, catch, so) ->
        check_statement fonction_mere s;
        begin
          match catch with
          | None -> ()
          | Some(n, s) ->
            check_statement fonction_mere s;
        end;
        begin
          match so with
          | None -> ()
          | Some s -> check_statement fonction_mere s
        end
     | Switch(x, cl) ->
                     let _ = check_expr fonction_mere x in
       List.iter
         begin fun (cl, s) ->
           List.iter
             begin function
             | Default -> ()
             | Case x -> check_expr fonction_mere x
             end
             cl;
           check_statement fonction_mere s
         end
         cl


         (** check_lhs_or_var *) 
  and check_lhs_or_var fonction_mere = function
  | LHS x -> check_expr_as_lhs fonction_mere x
  | Vars vl -> List.iter (check_variable_declaration fonction_mere) vl

  (** check_variable_declaration*)
  and check_variable_declaration fonction_mere (name, xo) =
      begin match xo with
        | None -> ()
        | Some x -> check_expr fonction_mere x
      end

let callgraph sources =
  List.iter
    begin fun s ->
      try
        List.iter (check_source_element "MAIN") s.s_source
      with  e -> failwith "erreur dont check_variable_declaration"
    end
    sources
;;
(* ***)
