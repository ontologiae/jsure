(*#require "aurochs_lib";;*)
#require "extlib";;
#require "str";;
print_endline "ast";;
#load "ast.cmo";;
print_endline "convert";;
#load "convert.cmo";;
print_endline "process";;

#load "process.cmo";;

(*#load "pffpsf.cmo";;
#load "ansi.cmo";;
#load "opt.cmo";;
#load "ast.cmo";;
#load "convert.cmo";;
#load "liner.cmo";;
#load "source.cmo";;
#load "excerpt.cmo";;
#load "liner.cmo";;
#load "ecma.cmo";;
#load "ecmarex.cmo";;
#load "generate.cmo";;
#load "qwerty.cmo";;
#load "levenshtein.cmo";;
#load "minefield.cmo";;
#load "conduit.cmo";;
#load "eval.cmo";;
#load "check.cmo";;
#load "process.cmo";;
#load "version.cmo";;*)
#print_length 10000;;
#print_depth 10000;;

open Ast;;

(*let src = Process.read_source (`File "tests/flowanalysis.js");;*)





let trace_flot_appel = 1;;








let flow_analysis = [St (0, 59, Position (0, 59, Variable [("array_a_la_con", Some (Array [Object [(PN_String "e", L (String "jlkj"))]; Object [(PN_String "e", L (String "azerty"))]; Object [(PN_String "e", L (String "123456"))]]))]));  
 St (62, 175,Position (62, 175,
   Variable
    [("find",
      Some
       (Function (73, 175,
         (None, ["elem"; "tab"],
          [St (95, 156,
            Position (95, 156,
             ForIn (Vars [("e", None)], V "tab",
              Position (113, 156,
               Block
                [Position (117, 153, If (B (B_physequal, V "e", V "elem"), Position (133, 153, Block [Position (138, 146, Return (Some (V "e"))); Position (150, 150, Nop)]), None));
                 Position (153, 153, Nop)]))));
           St (156, 172, Position (156, 172, Return (Some (L Undefined))))]))))]));
 St (179, 224, Position (179, 224, Variable [("une_var", Some (Apply (V "find", [Object [(PN_String "e", L (String "rien"))]; V "array_a_la_con"])))]));
 St (227, 243, Position (227, 243, Expr (Assign (B (B_bracket, V "une_var", L
 (String "truc")), A_eq, L (Float 2.)))))];;




let ast1 = [Ast.St (0, 11, Ast.Position (0, 11, Ast.Return (Some (Ast.L (Ast.Bool true)))));
Ast.FunDecl (13, 119, (Some "foo", ["x"; "y"; "z"],
   [Ast.St (37, 51, Ast.Position (37, 51, Ast.Expr (Ast.Assign (Ast.B (Ast.B_bracket, Ast.V "x", Ast.L (Ast.String "name")), Ast.A_eq, Ast.L (Ast.String "foo")))));
    Ast.St (55, 117,
     Ast.Position (55, 117,
      Ast.If (Ast.V "x", Ast.Position (61, 83, Ast.Block [Ast.Position (67, 81, Ast.Return (Some (Ast.L (Ast.Bool true)))); Ast.Position (81, 81, Ast.Nop)]),
       Some
        (Ast.Position (88, 117,
          Ast.Block
           [Ast.Position (94, 111, Ast.Return (Some (Ast.Apply (Ast.B (Ast.B_bracket, Ast.B (Ast.B_bracket, Ast.V "x", Ast.L (Ast.String "y")), Ast.L (Ast.String "z")), [Ast.V "a"; Ast.V "b"]))));
            Ast.Position (115, 115, Ast.Nop)])))))]));
 Ast.FunDecl (119, 154,
  (Some "f", ["x"], [Ast.St (135, 152, Ast.Position (135, 152, Ast.Return (Some
  (Ast.B (Ast.B_add, Ast.B (Ast.B_mul, Ast.L (Ast.Float 2.), Ast.L (Ast.Float
  1.)), Ast.L (Ast.Float 1.))))))]))];;



let asttest = [Ast.St (0, 9, Ast.Position (0, 9, Ast.Variable [("x", Some (Ast.L (Ast.Float 3.)))]));
Ast.St (11, 83,Ast.Position (11, 83,
   Ast.Variable
    [("x",
      Some
       (Ast.Object
         [(Ast.PN_String "foo", Ast.V "bar"); (Ast.PN_String "zonk", Ast.L (Ast.Float 33.)); 
          (Ast.PN_String "foobar", Ast.L (Ast.Bool true)); (Ast.PN_String "toto", Ast.L (Ast.String "shit")); 
          (Ast.PN_String "otot", Ast.L (Ast.String "shit"))]))]));
 Ast.St (85, 210,
  Ast.Position (85, 210,
   Ast.Variable
    [("gogol",
      Some
       (Ast.Object
         [(Ast.PN_String "fun",
           Ast.Object
            [(Ast.PN_String "bar", Ast.V "baz"); (Ast.PN_String "zonk", Ast.B (Ast.B_bracket, Ast.V "zink", Ast.L (Ast.Float 33.)));
             (Ast.PN_String "fifi", Ast.Function (152, 181, (None, [], [Ast.St (166, 178, Ast.Position (166, 178, Ast.Return (Some (Ast.L (Ast.Bool false)))))])));
             (Ast.PN_String "toutou", Ast.U (Ast.U_not, Ast.U (Ast.U_not, Ast.V "toutou")))])]))]));
 Ast.FunDecl (210, 244, (Some "foo", [], [Ast.St (229, 240, Ast.Position (229, 240, Ast.Return (Some (Ast.L (Ast.Bool true)))))]));
 Ast.FunDecl (244, 263, (Some "bar", [], [Ast.St (261, 261, Ast.Position (261, 261, Ast.Nop))]));
 Ast.St (263, 488,
  Ast.Position (263, 488,
   Ast.Variable
    [("Prototype",
      Some
       (Ast.Object
         [(Ast.PN_String "Version", Ast.L (Ast.String "1.5.0"));
          (Ast.PN_String "BrowserFeatures", Ast.Object [(Ast.PN_String "XPath", Ast.U (Ast.U_not, Ast.U (Ast.U_not, Ast.B (Ast.B_bracket, Ast.V "document", Ast.L (Ast.String "evaluate")))))]);
          (Ast.PN_String "ScriptFragment", Ast.L (Ast.String "(?:<script.*?>)((\\n|\\r|.)*?)(?:<\\/script>)"));
          (Ast.PN_String "emptyFunction", Ast.Function (440, 453, (None, [], [Ast.St (452, 452, Ast.Position (452, 452, Ast.Nop))])));
          (Ast.PN_String "K", Ast.Function (460, 485, (None, ["x"], [Ast.St (474, 483, Ast.Position (474, 483, Ast.Return (Some (Ast.V "x"))))])))]))]));
 Ast.St (488, 608,
  Ast.Position (488, 608,
   Ast.Variable
    [("Class",
      Some
       (Ast.Object
         [(Ast.PN_String "create",
           Ast.Function (512, 605,
            (None, [],
             [Ast.St (529, 603,
               Ast.Position (529, 603,
                Ast.Return
                 (Some
                   (Ast.Function (536, 603,
                     (None, [],
                      [Ast.St (555, 593,
                        Ast.Position (555, 593,
                         Ast.Expr (Ast.Apply (Ast.B (Ast.B_bracket, Ast.B (Ast.B_bracket, Ast.This, Ast.L (Ast.String "initialize")), Ast.L (Ast.String "apply")), [Ast.This; Ast.V "arguments"]))))]))))))])))]))]));
 Ast.St (608, 638, Ast.Position (608, 638, Ast.Variable [("Abstract", Some (Ast.U (Ast.U_new, Ast.Apply (Ast.V "Object", [Ast.L (Ast.Float 3.); Ast.L (Ast.Float 4.)]))))]));
 Ast.St (641, 797,
  Ast.Position (641, 797,
   Ast.Expr
    (Ast.Assign (Ast.B (Ast.B_bracket, Ast.V "Object", Ast.L (Ast.String "extend")), Ast.A_eq,
      Ast.Function (657, 797,
       (None, ["destination"; "source"],
        [Ast.St (691, 774,
          Ast.Position (691, 774,
           Ast.ForIn (Ast.Vars [("property", None)], Ast.V "source",
            Ast.Position (720, 774,
             Ast.Block
              [Ast.Position (726, 766, Ast.Expr (Ast.Assign (Ast.B (Ast.B_bracket, Ast.V "destination", Ast.V "property"), Ast.A_eq, Ast.B (Ast.B_bracket, Ast.V "source", Ast.V "property"))));
               Ast.Position (770, 770, Ast.Nop)]))));
         Ast.St (774, 792, Ast.Position (774, 792, Ast.Return (Some (Ast.V "destination"))))]))))));
 Ast.St (797, 1463,
  Ast.Position (797, 1463,
   Ast.Expr
    (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "Object", Ast.L (Ast.String "extend")),
      [Ast.V "Object";
       Ast.Object
        [(Ast.PN_String "inspect",
          Ast.Function (832, 1114,
           (None, ["object"],
            [Ast.St (855, 1113,
              Ast.Position (855, 1113,
               Ast.Try
                (Ast.Block
                  [Ast.Block
                    [Ast.Position (867, 919, Ast.If (Ast.B (Ast.B_physequal, Ast.V "object", Ast.L Ast.Undefined), Ast.Position (893, 911, Ast.Return (Some (Ast.L (Ast.String "undefined")))), None));
                     Ast.Position (919, 961, Ast.If (Ast.B (Ast.B_physequal, Ast.V "object", Ast.L Ast.Null), Ast.Position (940, 953, Ast.Return (Some (Ast.L (Ast.String "null")))), None));
                     Ast.Position (961, 1021,
                      Ast.Return
                       (Some
                         (Ast.Conditional (Ast.B (Ast.B_bracket, Ast.V "object", Ast.L (Ast.String "inspect")), 
                           Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "object", Ast.L (Ast.String "inspect")), []), 
                           Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "object", Ast.L (Ast.String "toString")), [])))));
                     Ast.Position (1027, 1027, Ast.Nop)]],
                Some
                 ("e",
                  Ast.Block
                   [Ast.Block
                     [Ast.Position (1047, 1096, Ast.If (Ast.B (Ast.B_instanceof, Ast.V "e", Ast.V "RangeError"), Ast.Position (1076, 1088, Ast.Return (Some (Ast.L (Ast.String "...")))), None));
                      Ast.Position (1096, 1103, Ast.Throw (Ast.V "e")); 
                      Ast.Position (1109, 1109, Ast.Nop)]]),
                None)))])));
         (Ast.PN_String "keys",
          Ast.Function (1125, 1243,
           (None, ["object"],
            [Ast.St (1148, 1161, Ast.Position (1148, 1161, Ast.Variable [("keys", Some (Ast.Array []))]));
             Ast.St (1167, 1221,
              Ast.Position (1167, 1221,
               Ast.ForIn (Ast.Vars [("property", None)], Ast.V "object", Ast.Position (1202, 1221, Ast.Expr (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "keys", Ast.L (Ast.String "push")), [Ast.V "property"]))))));
             Ast.St (1227, 1238, Ast.Position (1227, 1238, Ast.Return (Some (Ast.V "keys"))))])));
         (Ast.PN_String "values",
          Ast.Function (1256, 1388,
           (None, ["object"],
            [Ast.St (1279, 1294, Ast.Position (1279, 1294, Ast.Variable [("values", Some (Ast.Array []))]));
             Ast.St (1300, 1364,
              Ast.Position (1300, 1364,
               Ast.ForIn (Ast.Vars [("property", None)], Ast.V "object",
                Ast.Position (1335, 1364, Ast.Expr (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "values", Ast.L (Ast.String "push")), [Ast.B (Ast.B_bracket, Ast.V "object", Ast.V "property")]))))));
             Ast.St (1370, 1383, Ast.Position (1370, 1383, Ast.Return (Some (Ast.V "values"))))])));
         (Ast.PN_String "clone",
          Ast.Function (1400, 1461,
           (None, ["object"],
            [Ast.St (1423, 1455, Ast.Position (1423, 1455, Ast.Return (Some (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "Object", Ast.L (Ast.String "extend")), [Ast.Object []; Ast.V "object"])))))])))]]))));
 Ast.St (1466, 1665,
  Ast.Position (1466, 1665,
   Ast.Expr
    (Ast.Assign (Ast.B (Ast.B_bracket, Ast.B (Ast.B_bracket, Ast.V "Function", Ast.L (Ast.String "prototype")), Ast.L (Ast.String "bind")), Ast.A_eq,
      Ast.Function (1492, 1665,
       (None, [],
        [Ast.St (1507, 1571,
          Ast.Position (1507, 1571,
           Ast.Variable
            [("__method", Some Ast.This); ("args", Some (Ast.Apply (Ast.V "$A", [Ast.V "arguments"]))); ("object", Some (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "args", Ast.L (Ast.String "shift")), [])))]));
         Ast.St (1575, 1662,
          Ast.Position (1575, 1662,
           Ast.Return
            (Some
              (Ast.Function (1582, 1662,
                (None, [],
                 [Ast.St (1599, 1656,
                   Ast.Position (1599, 1656,
                    Ast.Return
                     (Some
                       (Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "__method", Ast.L (Ast.String "apply")),
                         [Ast.V "object"; Ast.Apply (Ast.B (Ast.B_bracket, Ast.V "args", Ast.L (Ast.String "concat")), [Ast.Apply (Ast.V "$A", [Ast.V "arguments"])])])))))]))))))]))))));
 Ast.St (1665, 1747,
  Ast.Position (1665, 1747,
   Ast.Expr
    (Ast.Assign (Ast.B (Ast.B_bracket, Ast.B (Ast.B_bracket, Ast.V "Function", Ast.L (Ast.String "prototype")), Ast.L (Ast.String "bindAsEventListener")), Ast.A_eq,
      Ast.Function (1706, 1747,
       (None, ["object"], [Ast.St (1727, 1743, Ast.Position (1727, 1743, Ast.Return (Some (Ast.Apply (Ast.B (Ast.B_bracket, Ast.Apply (Ast.V "f", [Ast.V "a"]), Ast.L (Ast.String "f")), [Ast.V "b"])))))]))))));
 Ast.St (1747, 1763, Ast.Position (1747, 1763, Ast.Variable [("a", Some (Ast.L (Ast.Float 1.))); ("b", Some (Ast.L (Ast.Float 2.)))]));
 Ast.St (1765, 1838,
  Ast.Position (1765, 1838,
   Ast.For (Some (Ast.Position (1769, 1805, Ast.Variable [("i", Some (Ast.L (Ast.Float 0.))); ("length", Some (Ast.B (Ast.B_bracket, Ast.V "arguments", Ast.L (Ast.String "length"))))])),
    Some (Ast.Position (1807, 1813, Ast.Expr (Ast.B (Ast.B_lt, Ast.V "i", Ast.L (Ast.Float 33.))))), 
    Some (Ast.Position (1815, 1819, Ast.Expr (Ast.U (Ast.U_post_increment, Ast.V "i")))),
    Ast.Position (1821, 1838, Ast.Block [Ast.Position (1825, 1833, Ast.Expr (Ast.Apply (Ast.V "print", [Ast.V "i"]))); Ast.Position (1835, 1835, Ast.Nop)]))));
 Ast.St (1838, 1931,
  Ast.Position (1838, 1931,
   Ast.Variable
    [("toto",
      Some
       (Ast.Object
         [(Ast.PN_String "onTimerEvent",
           Ast.Function (1867, 1930, (None, [], [Ast.St (1918, 1928, Ast.Position (1918, 1928, Ast.Block [Ast.Position (1924, 1924, Ast.Nop); Ast.Position (1924, 1924, Ast.Nop)]))])))]))]));
 Ast.St (1934, 1953, Ast.Position (1934, 1953, Ast.Variable [("gogol", Some (Ast.L (Ast.Regexp ("[a-z]", ""))))]));
 Ast.St (1956, 1969, Ast.Position (1956, 1969, Ast.Variable [("foo", Some (Ast.L (Ast.String " ")))])); 
 Ast.St (1972, 1984, Ast.Position (1972, 1984, Ast.Variable [("foo", Some (Ast.L (Ast.String "")))]));
 Ast.St (1987, 2003, Ast.Position (1987, 2003, Ast.Variable [("foo", Some (Ast.Array [Ast.L (Ast.Float 1.); Ast.L (Ast.Float 2.)]))]));
 Ast.FunDecl (2006, 2281,
  (Some "foo", [],
   [Ast.St (2025, 2070,
     Ast.Position (2025, 2070,
      Ast.Switch (Ast.V "a", [([Ast.Case (Ast.L (Ast.Float 3.))], Ast.Block [Ast.Position (2049, 2060, Ast.Return (Some (Ast.L (Ast.Bool true)))); Ast.Position (2064, 2064, Ast.Nop)])])));
    Ast.St (2070, 2279,
     Ast.Position (2070, 2279,
      Ast.For (Some (Ast.Position (2074, 2110, Ast.Variable [("i", Some (Ast.L (Ast.Float 0.))); ("length", Some (Ast.B (Ast.B_bracket, Ast.V "arguments", Ast.L (Ast.String "length"))))])),
       Some (Ast.Position (2112, 2122, Ast.Expr (Ast.B (Ast.B_lt, Ast.V "i", Ast.V "length")))), 
       Some (Ast.Position (2124, 2127, Ast.Expr (Ast.U (Ast.U_post_increment, Ast.V "i")))),
       Ast.Position (2129, 2279,
        Ast.Block
         [Ast.Position (2137, 2167, Ast.Variable [("value", Some (Ast.B (Ast.B_bracket, Ast.This, Ast.B (Ast.B_bracket, Ast.V "arguments", Ast.V "i"))))]);
          Ast.Position (2175, 2185, Ast.Return (Some (Ast.L (Ast.Float 1.))));
          Ast.Position (2193, 2246,
           Ast.If (Ast.V "x", Ast.Position (2200, 2231, Ast.Block [Ast.Position (2210, 2221, Ast.Return (Some (Ast.L (Ast.Bool true)))); Ast.Position (2229, 2229, Ast.Nop)]),
            Some (Ast.Position (2236, 2246, Ast.Block [Ast.Position (2238, 2238, Ast.Nop); Ast.Position (2238, 2238, Ast.Nop)]))));
          Ast.Position (2246, 2271, Ast.Expr (Ast.U (Ast.U_delete, Ast.B (Ast.B_bracket, Ast.This, Ast.B (Ast.B_bracket, Ast.V "arguments", Ast.V "i"))))); 
          Ast.Position (2277, 2277, Ast.Nop)]))))]))]



(*let (ast: program) = Marshal.from_string binCallGraph 0;; *)

let callgraphExemple = 
         [FunDecl (0, 58, (Some "f1", [], [St (17, 34, Position (17, 34, Variable [("something", Some (L (String "")))])); St (37, 53, Position (37, 53, Return (Some (V "something"))))]));
         FunDecl (58, 123, (Some "f2", [],
     [St (75, 92, Position (75, 92, Variable [("something", Some (L (String "")))]));
      St (95, 99, Position (95, 99, Expr (Apply (V "f1", [])))); St (102, 118, Position (102, 118, Return (Some (V "something"))))]));
   FunDecl (123, 188,
    (Some "f3", [],
     [St (140, 157, Position (140, 157, Variable [("something", Some (L (String "")))]));
      St (160, 164, Position (160, 164, Expr (Apply (V "f2", []))));
      St (167, 183, Position (167, 183, Return (Some (V "something"))))]));
   FunDecl (188, 253,
    (Some "f4", [],
     [St (205, 222, Position (205, 222, Variable [("something", Some (L (String "")))]));
      St (225, 229, Position (225, 229, Expr (Apply (V "f1", []))));
      St (232, 248, Position (232, 248, Return (Some (V "something"))))]));
   FunDecl (253, 326,
    (Some "f5", [],
     [St (270, 287, Position (270, 287, Variable [("something", Some (L (String "")))]));
      St (290, 294, Position (290, 294, Expr (Apply (V "f3", []))));
      St (297, 301, Position (297, 301, Expr (Apply (V "f2", []))));
      St (304, 320, Position (304, 320, Return (Some (V "something"))))]));
   FunDecl (326, 393,
    (Some "f6", [],
     [St (343, 360, Position (343, 360, Variable [("something", Some (L (String "")))]));
      St (363, 367, Position (363, 367, Expr (Apply (V "f5", []))));
      St (370, 386, Position (370, 386, Return (Some (V "something"))))]));
   FunDecl (393, 473,
    (Some "f7", [],
     [St (410, 414, Position (410, 414, Expr (Apply (V "f6", []))));
      St (417, 421, Position (417, 421, Expr (Apply (V "f5", []))));
      St (424, 428, Position (424, 428, Expr (Apply (V "f1", []))));
      St (431, 448, Position (431, 448, Variable [("something", Some (L (String "")))]));
      St (451, 467, Position (451, 467, Return (Some (V "something"))))]));
   FunDecl (473, 501, (Some "main", [], [St (492, 496, Position (492, 496, Expr (Apply (V "f7", []))))]))]
