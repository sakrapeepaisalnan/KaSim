(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type pervasives_bool = bool

type ('mix,'id) e =
    BIN_ALG_OP of Operator.bin_alg_op *
                  ('mix,'id) e Locality.annot * ('mix,'id) e Locality.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mix,'id) e Locality.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t
  | IF of ('mix,'id) bool Locality.annot *
          ('mix,'id) e Locality.annot * ('mix,'id) e Locality.annot
  | DIFF_TOKEN of (('mix,'id) e Locality.annot * 'id)
  | DIFF_KAPPA_INSTANCE of (('mix,'id) e Locality.annot * 'mix)
and ('mix,'id) bool =
  | TRUE
  | FALSE
  | BOOL_OP of
      Operator.bool_op *
      ('mix,'id) bool Locality.annot * ('mix,'id) bool Locality.annot
  | COMPARE_OP of Operator.compare_op *
                  ('mix,'id) e Locality.annot * ('mix,'id) e Locality.annot

type t = (Pattern.id array list, int) e

let rec e_to_yojson f_mix f_id = function
  | BIN_ALG_OP (op,a,b) ->
    `List [Operator.bin_alg_op_to_json op;
           Locality.annot_to_json (e_to_yojson f_mix f_id) a;
           Locality.annot_to_json (e_to_yojson f_mix f_id) b]
  | UN_ALG_OP (op,a) ->
    `List [Operator.un_alg_op_to_json op;
           Locality.annot_to_json (e_to_yojson f_mix f_id) a]
  | STATE_ALG_OP op -> Operator.state_alg_op_to_json op
  | ALG_VAR i -> `List [`String "VAR"; f_id i]
  | KAPPA_INSTANCE cc -> `List [`String "MIX"; f_mix cc]
  | TOKEN_ID i -> `List [`String "TOKEN"; f_id i]
  | CONST n -> Nbr.to_json n
  | IF (cond,yes,no) ->
    `List [`String "IF";
           Locality.annot_to_json (bool_to_yojson f_mix f_id) cond;
           Locality.annot_to_json (e_to_yojson f_mix f_id) yes;
           Locality.annot_to_json (e_to_yojson f_mix f_id) no]
  | DIFF_TOKEN (expr,token) ->
    `List [`String "DIFF_TOKEN";
           Locality.annot_to_json (e_to_yojson f_mix f_id) expr;
           f_id token]
  | DIFF_KAPPA_INSTANCE (expr,mixture) ->
    `List [`String "DIFF_MIXTURE";
           Locality.annot_to_json (e_to_yojson f_mix f_id) expr;
           f_mix mixture]

and bool_to_yojson f_mix f_id = function
  | TRUE -> `Bool true
  | FALSE -> `Bool false
  | BOOL_OP (op,a,b) ->
    `List [ Operator.bool_op_to_json op;
            Locality.annot_to_json (bool_to_yojson f_mix f_id) a;
            Locality.annot_to_json (bool_to_yojson f_mix f_id) b ]
  | COMPARE_OP (op,a,b) ->
    `List [ Operator.compare_op_to_json op;
            Locality.annot_to_json (e_to_yojson f_mix f_id) a;
            Locality.annot_to_json (e_to_yojson f_mix f_id) b ]

let rec e_of_yojson f_mix f_id = function
  | `List [`String "DIFF_MIXTURE"; expr ; mixture] ->
    DIFF_KAPPA_INSTANCE
      (Locality.annot_of_json (e_of_yojson f_mix f_id) expr,
       f_mix mixture)
  | `List [`String "DIFF_TOKEN"; expr ; tok] ->
    DIFF_TOKEN
      (Locality.annot_of_json (e_of_yojson f_mix f_id) expr,
       f_id tok)
  | `List [op;a;b] ->
    BIN_ALG_OP
      (Operator.bin_alg_op_of_json op,
       Locality.annot_of_json (e_of_yojson f_mix f_id) a,
       Locality.annot_of_json (e_of_yojson f_mix f_id) b)
  | `List [`String "VAR"; i] -> ALG_VAR (f_id i)
  | `List [`String "TOKEN"; i] -> TOKEN_ID (f_id i)
  | `List [`String "MIX"; cc] -> KAPPA_INSTANCE (f_mix cc)
  | `List [op;a] ->
    UN_ALG_OP (Operator.un_alg_op_of_json op,
               Locality.annot_of_json (e_of_yojson f_mix f_id) a)
  | `List [`String "IF"; cond; yes; no] ->
    IF (Locality.annot_of_json (bool_of_yojson f_mix f_id) cond,
        Locality.annot_of_json (e_of_yojson f_mix f_id) yes,
        Locality.annot_of_json (e_of_yojson f_mix f_id) no)
  | x ->
    try STATE_ALG_OP (Operator.state_alg_op_of_json x)
    with Yojson.Basic.Util.Type_error _ ->
    try  CONST (Nbr.of_json x)
    with Yojson.Basic.Util.Type_error _ ->
      raise (Yojson.Basic.Util.Type_error ("Invalid Alg_expr",x))
and bool_of_yojson f_mix f_id = function
  | `Bool b -> if b then TRUE else FALSE
  | `List [op; a; b] as x ->
    begin
      try BOOL_OP (Operator.bool_op_of_json op,
                   Locality.annot_of_json (bool_of_yojson f_mix f_id) a,
                   Locality.annot_of_json (bool_of_yojson f_mix f_id) b)
      with Yojson.Basic.Util.Type_error _ ->
      try COMPARE_OP (Operator.compare_op_of_json op,
                      Locality.annot_of_json (e_of_yojson f_mix f_id) a,
                      Locality.annot_of_json (e_of_yojson f_mix f_id) b)
      with Yojson.Basic.Util.Type_error _ ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect bool expr",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect bool_expr",x))

let rec print pr_mix pr_tok pr_var f = function
  | CONST n -> Nbr.print f n
  | ALG_VAR lab -> pr_var f lab
  | KAPPA_INSTANCE ast -> pr_mix f ast
  | TOKEN_ID tk -> Format.fprintf f "|%a|" pr_tok tk
  | STATE_ALG_OP op -> Operator.print_state_alg_op f op
  | BIN_ALG_OP (op, (a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)"
      (print pr_mix pr_tok pr_var) a
      Operator.print_bin_alg_op op
      (print pr_mix pr_tok pr_var) b
  | UN_ALG_OP (op, (a,_)) ->
    Format.fprintf f "%a(%a)" Operator.print_un_alg_op op
      (print pr_mix pr_tok pr_var) a
  | IF ((cond,_),(yes,_),(no,_)) ->
    Format.fprintf f "%a [?] %a [:] %a" (print_bool pr_mix pr_tok pr_var) cond
      (print pr_mix pr_tok pr_var) yes (print pr_mix pr_tok pr_var) no
  | DIFF_TOKEN ((expr,_), tok) ->
    Format.fprintf f "diff(%a,%a)" (print pr_mix pr_tok pr_var) expr pr_tok tok
  | DIFF_KAPPA_INSTANCE ((expr,_), mixture) ->
      Format.fprintf f "diff(%a,%a)" (print pr_mix pr_tok pr_var) expr pr_mix mixture
and print_bool pr_mix pr_tok pr_var f = function
  | TRUE -> Format.fprintf f "[true]"
  | FALSE -> Format.fprintf f "[false]"
  | BOOL_OP (op,(a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)" (print_bool pr_mix pr_tok pr_var) a
      Operator.print_bool_op op (print_bool pr_mix pr_tok pr_var) b
  | COMPARE_OP (op,(a,_), (b,_)) ->
    Format.fprintf f "(%a %a %a)"
      (print pr_mix pr_tok pr_var) a
      Operator.print_compare_op op
      (print pr_mix pr_tok pr_var) b

let const n = Locality.dummy_annot (CONST n)
let int i = const (Nbr.I i)
let float f = const (Nbr.F f)
let add e1 e2 = Locality.dummy_annot (BIN_ALG_OP (Operator.SUM,e1,e2))
let minus e1 e2 = Locality.dummy_annot (BIN_ALG_OP (Operator.MINUS,e1,e2))
let mult e1 e2 = Locality.dummy_annot (BIN_ALG_OP (Operator.MULT,e1,e2))
let div e1 e2 = Locality.dummy_annot (BIN_ALG_OP (Operator.DIV,e1,e2))
let pow e1 e2  = Locality.dummy_annot (BIN_ALG_OP (Operator.POW,e1,e2))
let log e1 = Locality.dummy_annot (UN_ALG_OP (Operator.LOG,e1))
let ln e1 = (* JF: If I rememnber well *)
  div
    (log e1)
    (log (int 10))
let sin e1 = Locality.dummy_annot (UN_ALG_OP (Operator.SINUS,e1))
let cos e1 = Locality.dummy_annot (UN_ALG_OP (Operator.COSINUS,e1))
let uminus e1 = Locality.dummy_annot (UN_ALG_OP (Operator.UMINUS,e1))
let sqrt e1 = Locality.dummy_annot (UN_ALG_OP (Operator.SQRT,e1))

let rec add_dep (in_t,in_e,toks_d,out as x) d = function
  | BIN_ALG_OP (_, a, b), _ -> add_dep (add_dep x d a) d b
  | (UN_ALG_OP (_, a) | DIFF_TOKEN (a,_) | DIFF_KAPPA_INSTANCE (a,_)), _
(* when we differentiate against a variable, the result may depend on this variable only if the variable occurs in the differntiated expression *)
    -> add_dep x d a
  | ALG_VAR j, _ ->
    let () = out.(j) <- Operator.DepSet.add d out.(j) in
    x
  | (KAPPA_INSTANCE _ | CONST _), _ -> x
  | TOKEN_ID i, _ ->
    let () = toks_d.(i) <- Operator.DepSet.add d toks_d.(i) in
    x
  | IF (cond,yes,no), _ -> add_dep (add_dep (add_dep_bool x d cond) d yes) d no
  | STATE_ALG_OP op, _ ->
    match op with
    | (Operator.EMAX_VAR | Operator.TMAX_VAR) -> x
    | Operator.TIME_VAR -> (Operator.DepSet.add d in_t,in_e,toks_d,out)
    | (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR) ->
      (in_t,Operator.DepSet.add d in_e,toks_d,out)

and add_dep_bool x d = function
  | (TRUE | FALSE), _ -> x
  | BOOL_OP (_,a, b), _ -> add_dep_bool (add_dep_bool x d a) d b
  | COMPARE_OP (_,a, b), _ -> add_dep (add_dep x d a) d b

let rec has_mix :
  type a. ?var_decls:('b -> ('c,'b) e) -> (a,'b) e -> pervasives_bool =
  fun ?var_decls -> function
  | BIN_ALG_OP (_, (a,_), (b,_)) -> has_mix ?var_decls a || has_mix ?var_decls b
  | UN_ALG_OP (_, (a,_))
  | DIFF_TOKEN ((a,_),_) | DIFF_KAPPA_INSTANCE ((a,_),_)  ->
    (* when we differentiate against a variable, the result may depend on this variable only if the variable occurs in the differntiated expression *)
    has_mix ?var_decls a
  | STATE_ALG_OP _ | CONST _ -> false
  | TOKEN_ID _ | KAPPA_INSTANCE _ -> true
  | IF ((cond,_),(yes,_),(no,_)) ->
    has_mix ?var_decls yes || has_mix ?var_decls no
    || bool_has_mix ?var_decls cond
  | ALG_VAR i ->
    match var_decls with
    | None -> false
    | Some f -> has_mix ?var_decls (f i)
and bool_has_mix :
  type a. ?var_decls:('b -> ('c,'b) e) -> (a,'b) bool -> pervasives_bool =
  fun ?var_decls -> function
  | TRUE | FALSE -> false
  | COMPARE_OP (_,(a,_),(b,_)) ->
    has_mix ?var_decls a || has_mix ?var_decls b
  | BOOL_OP (_,(a,_),(b,_)) ->
    bool_has_mix ?var_decls a || bool_has_mix ?var_decls b

let rec aux_extract_cc acc = function
  | BIN_ALG_OP (_, a, b), _ -> aux_extract_cc (aux_extract_cc acc a) b
  | (UN_ALG_OP (_, a) | DIFF_TOKEN (a,_) | DIFF_KAPPA_INSTANCE (a,_)),_ ->
    aux_extract_cc acc a
  | (ALG_VAR _ | CONST _ | TOKEN_ID _ | STATE_ALG_OP _), _ -> acc
  | KAPPA_INSTANCE i, _ -> i :: acc
  | IF (cond,yes,no), _ ->
    aux_extract_cc (aux_extract_cc (extract_cc_bool acc cond) yes) no
and extract_cc_bool acc = function
  | (TRUE | FALSE), _ -> acc
  | BOOL_OP (_,a, b), _ -> extract_cc_bool (extract_cc_bool acc a) b
  | COMPARE_OP (_,a, b), _ -> aux_extract_cc (aux_extract_cc acc a) b

let extract_connected_components x = aux_extract_cc [] x
let extract_connected_components_bool x = extract_cc_bool [] x

let setup_alg_vars_rev_dep toks vars =
  let in_t = Operator.DepSet.empty in
  let in_e = Operator.DepSet.empty in
  let toks_d = Array.make (NamedDecls.size toks) Operator.DepSet.empty in
  let out = Array.make (Array.length vars) Operator.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Operator.ALG i) y) (in_t,in_e,toks_d,out) vars

let rec propagate_constant ?max_time ?max_events updated_vars vars = function
  | BIN_ALG_OP (op,a,b),pos as x ->
    (match propagate_constant ?max_time ?max_events updated_vars vars a,
           propagate_constant ?max_time ?max_events updated_vars vars b with
    | (CONST c1,_),(CONST c2,_) -> CONST (Nbr.of_bin_alg_op op c1 c2),pos
    | ((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
       | DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _ | TOKEN_ID _ | ALG_VAR _
       | CONST _ | IF _),_ as a'),
      ((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
       | DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _  | TOKEN_ID _ | ALG_VAR _
       | CONST _ | IF _),_ as b') ->
      if a == a' && b == b' then x else (BIN_ALG_OP (op,a',b'),pos))
  | UN_ALG_OP (op,a),pos as x ->
    (match propagate_constant ?max_time ?max_events updated_vars vars a with
     | CONST c,_ -> CONST (Nbr.of_un_alg_op op c),pos
     | (DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _
       | BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _
       | KAPPA_INSTANCE _ | TOKEN_ID _ | ALG_VAR _ | IF _),_ as a' ->
    if a == a' then x else (UN_ALG_OP (op,a'),pos))
  | DIFF_TOKEN (a,t),pos as x ->
    (match propagate_constant ?max_time ?max_events updated_vars vars a with
     | CONST _,_ ->
       (* the derivative of a constant is zero *)
       CONST (Nbr.zero),pos
     | (DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _ | BIN_ALG_OP _ | UN_ALG_OP _ | IF _
       | STATE_ALG_OP _ | KAPPA_INSTANCE _ | TOKEN_ID _ | ALG_VAR _),_ as a' ->
       if a == a' then x else (DIFF_TOKEN (a',t),pos))
  | DIFF_KAPPA_INSTANCE (a,m),pos as x ->
    (match propagate_constant ?max_time ?max_events updated_vars vars a with
     | CONST _,_ ->
       (* the derivative of a constant is zero *)
       CONST (Nbr.zero),pos
     | (DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _ | BIN_ALG_OP _ | UN_ALG_OP _
       | STATE_ALG_OP _ | KAPPA_INSTANCE _ | TOKEN_ID _ | ALG_VAR _ | IF _),_ as a' ->
       if a == a' then x else (DIFF_KAPPA_INSTANCE (a',m),pos))
  | STATE_ALG_OP (Operator.EMAX_VAR),pos ->
    CONST
      (match max_events with
       | Some n -> Nbr.I n
       | None ->
         let () =
           ExceptionDefn.warning
             ~pos (fun f -> Format.pp_print_string
                      f "[Emax] constant is evaluated to infinity") in
         Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.TMAX_VAR),pos ->
    CONST
       (match max_time with
         | Some t -> Nbr.F t
         | None ->
           let () =
             ExceptionDefn.warning
               ~pos (fun f -> Format.pp_print_string
                        f "[Tmax] constant is evaluated to infinity") in
           Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
                 | Operator.NULL_EVENT_VAR),_ as x -> x
  | ALG_VAR i,pos as x ->
    (if List.mem i updated_vars then x
     else match vars.(i) with
       | _,((CONST _ | ALG_VAR _ as y),_) -> y,pos
       | _,((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
            | TOKEN_ID _ | IF _ | DIFF_KAPPA_INSTANCE _ | DIFF_TOKEN _),_) -> x)
  | (KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_ as x -> x

  | IF (cond,yes,no),pos ->
    match propagate_constant_bool
            ?max_time ?max_events updated_vars vars cond with
    | TRUE, _ ->
      propagate_constant ?max_time ?max_events updated_vars vars yes
    | FALSE,_ ->
      propagate_constant ?max_time ?max_events updated_vars vars no
    | (BOOL_OP _ | COMPARE_OP _),_ as cond' ->
      (IF (cond', propagate_constant ?max_time ?max_events updated_vars vars yes,
           propagate_constant ?max_time ?max_events updated_vars vars no),pos)
and propagate_constant_bool
    ?max_time ?max_events updated_vars vars = function
  | (TRUE | FALSE),_ as x -> x
  | BOOL_OP (op,a,b),pos ->
    begin match propagate_constant_bool
                  ?max_time ?max_events updated_vars vars a, op with
      | (TRUE,_), Operator.OR -> TRUE,pos
      | (FALSE,_), Operator.AND -> FALSE,pos
      | (TRUE,_), Operator.AND
      | (FALSE,_), Operator.OR ->
        propagate_constant_bool ?max_time ?max_events updated_vars vars b
      | ((BOOL_OP _ | COMPARE_OP _),_ as a'),_ ->
        match propagate_constant_bool
                ?max_time ?max_events updated_vars vars b, op with
        | (TRUE,_), Operator.OR -> TRUE,pos
        | (FALSE,_), Operator.AND -> FALSE,pos
        | (TRUE,_), Operator.AND
        | (FALSE,_), Operator.OR -> a'
        | ((BOOL_OP _ | COMPARE_OP _),_ as b'),_ ->
          BOOL_OP (op,a',b'),pos
    end
  | COMPARE_OP (op,a,b),pos ->
    let a' = propagate_constant ?max_time ?max_events updated_vars vars a in
    let b' = propagate_constant ?max_time ?max_events updated_vars vars b in
    match a',b' with
    | (CONST n1,_), (CONST n2,_) ->
      (if Nbr.of_compare_op op n1 n2 then TRUE,pos else FALSE,pos)
    | (( DIFF_KAPPA_INSTANCE _ | DIFF_TOKEN _
       | BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
       | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _ | IF _),_), _ ->
      COMPARE_OP (op,a',b'),pos

let rec has_time_dep (in_t,_,_,deps as vars_deps) = function
  | (BIN_ALG_OP (_, a, b),_) ->
    has_time_dep vars_deps a||has_time_dep vars_deps b
  | ((UN_ALG_OP (_, a) | DIFF_TOKEN (a,_) | DIFF_KAPPA_INSTANCE (a,_)),_) ->
    has_time_dep vars_deps a
  | ((KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_) -> false
  | (STATE_ALG_OP Operator.TIME_VAR,_) -> true
  | (STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
                   Operator.NULL_EVENT_VAR | Operator.EMAX_VAR |
                   Operator.TMAX_VAR),_) -> false
  | (ALG_VAR i,_) ->
    let rec aux j =
      Operator.DepSet.mem (Operator.ALG j) in_t ||
      Operator.DepSet.exists
        (function Operator.ALG k -> aux k
                | (Operator.RULE _ | Operator.PERT _) -> false) deps.(j) in
    aux i
  | IF (cond,yes,no),_ ->
    bool_has_time_dep vars_deps cond ||
    has_time_dep vars_deps yes||has_time_dep vars_deps no

and bool_has_time_dep vars_deps = function
  | (TRUE | FALSE), _ -> false
  | COMPARE_OP (_,a,b),_ ->
    has_time_dep vars_deps a||has_time_dep vars_deps b
  | BOOL_OP (_,a,b),_ ->
    bool_has_time_dep vars_deps a||bool_has_time_dep vars_deps b

let rec stops_of_bool vars_deps = function
  | TRUE | FALSE -> []
  | BOOL_OP (op,(a,_),(b,_)) ->
    let st1 = stops_of_bool vars_deps a in
    let st2 = stops_of_bool vars_deps b in
    (match op,st1,st2 with
     | _, [], _ -> st2
     | _, _, [] -> st1
     | Operator.OR, n1, n2 -> n1 @ n2
     | Operator.AND, _, _ -> raise ExceptionDefn.Unsatisfiable
    )
  | COMPARE_OP (op,(a1,_ as a),(b1,_ as b)) ->
    match op with
    | Operator.EQUAL when has_time_dep vars_deps a||has_time_dep vars_deps b ->
      begin match a1,b1 with
        | STATE_ALG_OP (Operator.TIME_VAR), CONST n
        | CONST n, STATE_ALG_OP (Operator.TIME_VAR) -> [n]
        | ( BIN_ALG_OP _ | UN_ALG_OP _ | ALG_VAR _
          | DIFF_TOKEN _ | DIFF_KAPPA_INSTANCE _
          | STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
                          Operator.TIME_VAR | Operator.NULL_EVENT_VAR |
                          Operator.EMAX_VAR |Operator.TMAX_VAR)
          | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _ | IF _), _ ->
          raise ExceptionDefn.Unsatisfiable
      end
    | (Operator.EQUAL | Operator.SMALLER | Operator.GREATER | Operator.DIFF) -> []
