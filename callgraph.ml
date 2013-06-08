(* Check *)

open Ast;;
open Source;;
open Liner;;

module H = BatHashtbl;;
module L = BatList;;









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
                     | Feuille a as f -> f ;;


let group_berenger cmp = function
  | [] -> []
  | l ->
    let rev_cmp a b = cmp b a in
    let sorted = List.sort rev_cmp l in
    match sorted with
      | x :: xs ->
        let local, global, _ =
          L.fold_left
            (fun (local_acc, global_acc, last) y ->
              if 0 = cmp last y then
                (y :: local_acc, global_acc, y)
              else
                ([y], local_acc :: global_acc, y)
            )
            ([x], [], x)
            xs
        in
        local :: global
      | _ -> failwith "UNREACHABLE STATEMENT"


let fusionne l = 
        let equalNode a b =
                match a, b with
                | Feuille a, Feuille b -> a = b
                | Noeud(a,b), Noeud(c,d) -> a = c 
                | Noeud (_, _), Feuille _ -> false 
                | Feuille _, Noeud (_, _) -> false in
        let groupes = group_berenger (fun a -> fun b -> if equalNode a  b then 0 else -1) l in
        List.map combineOne groupes;;
                

let (hash_verif_recursiv_arbre : (string,bool) H.t) =  H.create 1024 ;;


let donne_papa hid nom_fonction =  H.find_all hid nom_fonction ;;

let rec combineTree arb  =
        match arb with
        | Feuille a as b  ->  b
        | Noeud (a,b)     ->  Noeud(a, fusionne b);;


let rec parcour action arb  =
        let arb2 = action arb in
        match arb2 with
        | Feuille a as b  ->  b
        | Noeud (a,b)     ->  Noeud(a, List.map (parcour action ) b);;

let rec cherche_element_arbre_et_execute predicat action arbre =
        let lsta e = match e with
        | Feuille a  -> []
        | Noeud(_,l) -> l in

        let vasy arb =  match arb with
                | Feuille a as b   -> b
                | Noeud (a,b) as c -> let sousa = lsta c in Noeud(a, List.map (cherche_element_arbre_et_execute predicat action ) sousa) in

    match predicat arbre with
      | true -> let nouvel_arbre = action arbre in vasy nouvel_arbre 
      | false -> vasy arbre ;;


let rec reduce_arbre f val_init arbre =
         let lsta e = match e with
    | Feuille a  -> []
    | Noeud(_,l) -> l in

        let red = reduce_arbre f in
        match lsta arbre with
        | [] -> f val_init arbre 
        |  l -> List.fold_left red (f val_init arbre) l


let rec cherche_element_arbre f arbre =
  verifie_element f arbre
and verifie_element f el = 
  let lsta e = match e with
    | Feuille a  -> []
    | Noeud(_,l) -> l in

  match f el with
    | true  ->  true (*On l'a trouvé, on sort*)
      (* on ne l'a pas trouvé, on cherche dans les enfants, en rappelant verifie_element, il va aller au bout de l'arbre*)
    | false -> let lstfiltr =  L.filter (fun el ->  verifie_element f el ) (lsta el) in
               if L.length lstfiltr > 0 then
                   (*On renvoi le premier qu'on a trouvé*)
                 verifie_element f (L.hd lstfiltr)
               else false;;


let rec construit_arbre_appel (hid : (string, func_call) H.t)  racine =
  let papas = List.filter (fun e -> e.pere != e.fun_call) (donne_papa hid racine.pere) in
  match racine with
    | { pere = p  ; fun_call = sss } -> 
        match p = sss with (*récursive simple*)
          | true  -> Feuille racine.pere
          | false ->
              (match papas with
                | []    -> Feuille racine.pere
                | lst   -> Noeud  (racine.pere,  (L.map (construit_arbre_appel hid) lst))
              )




let rec check_rec  (hpid : (string, func_call) H.t) 
    (hid : (string, func_call) H.t) 
    (nom_fonction : string) =
  let nodes_fonction                     =  ( BatList.unique (H.find_all hpid nom_fonction) ) in 
  match  L.exists (fun e -> e.pere = e.fun_call) nodes_fonction with
    | true  -> true (** elle s'appelle elle même*)
    | false -> (** on cherche dans son arbre d'appel inversé si on ne l'a trouve pas*)
        let arbres_d_appels_inverses =   (L.map (construit_arbre_appel hid)  nodes_fonction) in 

        L.length ( L.map (cherche_element_arbre (fun e -> match e with
          | Feuille a   -> a = nom_fonction
          | Noeud (b,_) -> b = nom_fonction )
        )
                     arbres_d_appels_inverses)  > L.length arbres_d_appels_inverses;;




(*construit_arbre**** <-- {pere = "p.cherche_tous_dans_arbo"; fun_call = "func"}
construit_arbre**** <-- {pere = "func"; fun_call = ""}
construit_arbre**** <-- {pere = "_.flatten"; fun_call = ""}
construit_arbre**** <-- {pere = "_.map"; fun_call = ""}
construit_arbre**** <-- {pere = "p.cherche_tous_dans_arbo"; fun_call = "func"}*)

let rec construit_arbre hpid hid noeudss feuilles dejavu node =
  let profondeur_max  = 8 in
  let newdejavu       = BatList.unique (node::dejavu) in
  let r               =  string_of_int (Random.int 100) in
  (* je cherche dans la H, toutes les fonctions déclarées ayant le nom de fun_call*)
  match node with
    | { pere = p  ; fun_call = ""} -> Feuille p
    | { pere = p  ; fun_call = sss } -> 
        match p = sss with 
          | true  -> Feuille p
          | false ->

              let  sous_arbre     = match check_rec hpid hid node.fun_call  with
                | true  -> (*print_endline ("Cas de récursion à l'ordre "^string_of_int profondeur_cour )*) [Feuille node.fun_call ]
                | false -> 

                    let liste_nodes =  (BatList.remove_all (BatList.flatten (BatList.map (fun elem -> BatList.filter (fun e -> elem.fun_call = e.pere) (noeudss@feuilles)) (H.find_all hpid node.fun_call))) node) in
                                               (*let liste_node_nettoyee = BatList.flatten (BatList.map (fun a -> BatList.remove_all liste_node a) dejavu)*)
                    (*let _  = print_endline (String.concat " ; " (L.map (fun e-> "{pere=\""^e.pere^"\"; fun_call=\""^e.fun_call^"\"}") liste_nodes)) in*)
                    let res =        BatList.unique (
                      BatList.map (construit_arbre hpid hid noeudss feuilles newdejavu ) (L.filter (fun e-> not (e.pere = node.pere)) liste_nodes) (*on vire les nodes qui correspondent à la même fonction*)
                    (*c d'ailleurs gràce à ça qu'on pourrait tester si ya récursion*)
                    ) in

                    res in
              match BatList.exists ( fun e -> e.pere = node.pere) noeudss with(* verif que c bien une fonction qui appelle au moins une fonction, un node.pere quoi*)
                | false ->  Feuille node.pere
                | true  ->  (*if check_recursive hpid hid node.fun_call ||  BatList.exists (fun e -> e = node ) dejavu  then*)  Noeud (node.pere, BatList.unique (fusionne sous_arbre)) (*else Feuille
                                                                                                                                                                                          ("RECURSION"^r)*);;






let arborify arbolist =
  let _     = print_endline "Arborifyng" in
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
  fusionne (BatList.unique (List.map (construit_arbre hpid hid noeuds feuilles [] ) racines))




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
      
      let add = addFunc fonction_mere in
      let _  = 
        match x with
        | V n ->  add n
          | Function (i,j, (None, _,_)) -> (add "anonymous")
          | B (B_bracket, This, L (String str)) -> add ("this."^str)
          | B (B_bracket, V receveur, L (String message)) ->  add (receveur^"."^message)
          | B (B_bracket, Apply (V receveur, [L (String argument)]), L (String methode)) -> add (receveur^"("^argument^")"^"."^methode)
          | B (B_bracket, Apply (V receveur, [V argument]), L (String methode))          -> add (receveur^"("^argument^")"^"."^methode)
          | _   -> add ("_??_"^fonction_mere) in
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
          | None   ->  fonction_mere (*on considère que même si c'est une fonction interne, c'est la fonction mère qui nous intéresse, on va pas suivre les lambda à la trace*)
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



let rec really_read fd buffer start length =
  if length <= 0 then () else
    match Unix.read fd buffer start length with
      | 0 -> raise End_of_file
      | r -> really_read fd buffer (start + r) (length - r);;


let file2string path =
  let chan = try open_in path with Sys_error e -> print_endline ("Erreur d'ouverture du fichier :"^path);failwith "Erreur fichier" in
  let d = Unix.openfile path [Unix.O_RDONLY] 0o644 in
  let t = in_channel_length chan in
  let buffer = close_in chan; String.make t '*' in
  let b = really_read d buffer 0 t in
  buffer;;

let affiche_point() = output_string stdout ".";  flush stdout ;;  


let from_bin f =
  let  binCallGraph = file2string f in
  let (ast: program) =  affiche_point(); Marshal.from_string binCallGraph 0 in
  let source = {
    s_file = "";
    s_text = "";
    s_source = ast;
    s_liner =  {
      l_length = 0;
      l_table = [|8,8|];
      l_offset = 8
    };
    s_ignorify = false;
    s_warnify = false;
  }
  in 
  let _ =  affiche_point(); callgraph [source] in
  affiche_point(); (!^ dyn_call_list);;


let treePrint f = 
        let calls = from_bin f in
        let alls   = arborify calls in
        let reduce_arb = 
                let firsts = fusionne alls in List.map combineTree firsts in
        let strs  =  affiche_point();List.map (to_string 0) reduce_arb in
        let _     = print_endline "" in
        List.iter print_endline strs;;



let treePrint sources = 
  let _     =  affiche_point(); callgraph sources in
  let calls =  affiche_point(); (!^ dyn_call_list) in
  let alls   = arborify calls in
        let reduce_arb = 
                let firsts = fusionne alls in List.map combineTree firsts in
        let strs  =  affiche_point();List.map (to_string 0) reduce_arb in
        let _     = print_endline "" in
        List.iter print_endline strs;;

let treeString f =    
  let calls = from_bin f in
  let alls   = arborify calls in
        let reduce_arb =  
                let firsts = fusionne alls in List.map combineTree firsts in
        let strs  =  affiche_point();List.map (to_string 0)  reduce_arb in
        let _     = Gc.full_major() in 
        String.concat "\n" strs;;




(* ***)
