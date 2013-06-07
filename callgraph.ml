(* Check *)

open Ast;;
open Source;;
open Liner;;

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


let (hash_verif_recursiv_arbre : (string,bool) H.t) =  H.create 1024 ;;



let rec check_recursive 
                (hpid : (string, func_call) H.t) 
                (hid : (string, func_call) H.t) 
                (nom_fonction : string) = (*répond à la question : Est-ce que cette fonction appelée appelle une fonction qui fait partie des n pères de cette fonction*)
                (* la fonction  est-elle appelée dans une fonction ?)*) 
                let nodes_fonction                     =  ( BatList.unique (H.find_all hpid nom_fonction) ) in
                (*Renvoi la liste des fonction qui appellent cette fonction*)
                let check_pere_appelle fonction       = 
                                                        let peres_possibles = H.find_all hid fonction in
                                                        BatList.unique (BatList.filter (fun e -> e.fun_call = fonction) peres_possibles) in (* un peu stupide...*)
                let liste_des_peres_de fonction       =
                         let rec rec_liste_des_peres_de (fonction : func_call)   = 
                                        let l = check_pere_appelle fonction.pere in
                                        match l with
                                        | []    -> []
                                        | t::q  ->  (BatList.unique (check_pere_appelle fonction.pere))@(BatList.unique (BatList.flatten (BatList.map (fun e -> check_pere_appelle e.pere) q))) 
                in try BatList.flatten (BatList.map rec_liste_des_peres_de (BatList.unique (H.find_all hpid nom_fonction)) ) with Not_found -> [] 
                in
                try H.find hash_verif_recursiv_arbre nom_fonction
                with Not_found -> 
                        match nodes_fonction with
                        | []          -> false (* C'est bon, pas récursive*)
                        | fonctions   -> (** on récupère les pères possibles *)
                                        let peres = BatList.unique (BatList.flatten (BatList.map liste_des_peres_de fonctions)) in
                                        (* Soit Ni les nodes représentant la fonction (node.pere) dont on vérifie qu'elle est pas recursive
                                         * Soit Pi, la liste des nodes n pères des Ni
                                         * On veut savoir si Exist k,l tq Pk.fun_call = Nl.pere  *)
                                        try let res = BatList.length (BatList.filter (fun pere -> BatList.exists (fun node -> node.pere =  pere.fun_call) fonctions) peres) > 0 in
                                            let _   = H.add hash_verif_recursiv_arbre nom_fonction res in
                                            res
                                        with Not_found -> let _   = H.add hash_verif_recursiv_arbre nom_fonction false in false

(*construit_arbre**** <-- {pere = "p.cherche_tous_dans_arbo"; fun_call = "func"}
construit_arbre**** <-- {pere = "func"; fun_call = ""}
construit_arbre**** <-- {pere = "_.flatten"; fun_call = ""}
construit_arbre**** <-- {pere = "_.map"; fun_call = ""}
construit_arbre**** <-- {pere = "p.cherche_tous_dans_arbo"; fun_call = "func"}*)

let rec construit_arbre hpid hid noeudss feuilles dejavu node =
                let profondeur_max  = 8 in
                let newdejavu       = node::dejavu in
                let r               =  string_of_int (Random.int 100) in
                (* je cherche dans la H, toutes les fonctions déclarées ayant le nom de fun_call*)
                let  sous_arbre     = match check_recursive hpid hid node.fun_call ||  BatList.exists (fun e -> e = node ) dejavu with
                                        | true  -> (*print_endline ("Cas de récursion à l'ordre "^string_of_int profondeur_cour )*) [Feuille "RECURSION"]
                                        | false -> BatList.unique (
                                List.map (construit_arbre hpid hid noeudss feuilles newdejavu ) 
                                         (BatList.flatten (BatList.map (fun elem -> BatList.filter (fun e -> elem.fun_call = e.pere) (noeudss@feuilles)) (H.find_all hpid node.pere)))
                                )  in
                match BatList.exists ( fun e -> e.pere = node.pere) noeudss with
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
                    let nom_appel   = 
                            match x with
                            | V n -> n
                            | Function (i,j, (None, _,_)) -> "anonymous"
                            | B (B_bracket, This, L (String str)) -> "this."^str
                            | B (B_bracket, V receveur, L (String message)) -> receveur^"."^message 
                            | B (B_bracket, Apply (V receveur, [L (String argument)]), L (String methode)) -> receveur^"("^argument^")"^"."^methode
                            | B (B_bracket, Apply (V receveur, [V argument]), L (String methode))          -> receveur^"("^argument^")"^"."^methode
                            | _   -> "__" in
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




let treePrint sources = 
         let _     =  affiche_point(); callgraph sources in
         let calls =  affiche_point(); (!^ dyn_call_list) in
         let strs  =  affiche_point(); List.map (to_string 0) (arborify calls) in
        List.iter print_endline strs;;

let treeString f =    
        let calls = from_bin f in
        let strs  =  affiche_point();List.map (to_string 0) (arborify calls) in
        let _     = Gc.full_major() in 
        String.concat "\n" strs;;


(* ***)
