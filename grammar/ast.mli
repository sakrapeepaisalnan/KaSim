(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Kappa AST just after parsing *)

type ('a,'annot) link =
  | ANY_FREE
  | LNK_VALUE of int * 'annot
  | LNK_FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a * 'a (** port * agent_type *)

type internal = string Locality.annot list

type port = {
  port_nme:string Locality.annot;
  port_int:internal;
  port_int_mod: string Locality.annot option;
  port_lnk:(string Locality.annot,unit) link Locality.annot list;
  port_lnk_mod: int Locality.annot option option;
}

type counter_test = CEQ of int | CGTE of int | CVAR of string

type counter = {
  count_nme: string Locality.annot;
  count_test: counter_test Locality.annot option;
  count_delta: int Locality.annot;
}

type site =
  | Port of port
  | Counter of counter

type agent_mod = Erase | Create

type agent = (string Locality.annot * site list * agent_mod option)

type mixture = agent list

type edit_rule = {
  mix: mixture;
  delta_token: ((mixture,string) Alg_expr.e Locality.annot *
                string Locality.annot) list;
  act: (mixture,string) Alg_expr.e Locality.annot;
  un_act:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
}

type rule = {
  lhs: mixture ;
  rm_token: ((mixture,string) Alg_expr.e Locality.annot *
             string Locality.annot) list;
  bidirectional:bool ;
  rhs: mixture ;
  add_token: ((mixture,string) Alg_expr.e Locality.annot *
              string Locality.annot) list;
  k_def: (mixture,string) Alg_expr.e Locality.annot ;
  k_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*k_1:radius_opt*)
  k_op: (mixture,string) Alg_expr.e Locality.annot option ;
  k_op_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*rate for backward rule*)
}

val flip_label : string -> string

type ('mixture,'id) modif_expr =
  | INTRO of
      (('mixture,'id) Alg_expr.e Locality.annot * 'mixture Locality.annot)
  | DELETE of
      (('mixture,'id) Alg_expr.e Locality.annot * 'mixture Locality.annot)
  | UPDATE of
      ('id Locality.annot * ('mixture,'id) Alg_expr.e Locality.annot)
  (*TODO: pause*)
  | UPDATE_TOK of
      ('id Locality.annot * ('mixture,'id) Alg_expr.e Locality.annot)
  (*TODO: pause*)
  | STOP of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | SNAPSHOT of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      (('mixture,'id) Alg_expr.e Primitives.print_expr list) *
       (('mixture,'id) Alg_expr.e Primitives.print_expr list)
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Locality.annot)
  | CFLOWMIX of (bool * 'mixture Locality.annot)
  | FLUX of
      Primitives.flux_kind * ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | FLUXOFF of ('mixture,'id) Alg_expr.e Primitives.print_expr list
  | SPECIES_OF of
      (bool * ('mixture,'id) Alg_expr.e Primitives.print_expr list
       * 'mixture Locality.annot)

type ('mixture,'id) perturbation =
  (('mixture,'id) Alg_expr.bool Locality.annot *
   (('mixture,'id) modif_expr list) *
   ('mixture,'id) Alg_expr.bool Locality.annot option)
    Locality.annot

type configuration = string Locality.annot * (string Locality.annot list)

type ('mixture,'id) variable_def =
  string Locality.annot * ('mixture,'id) Alg_expr.e Locality.annot

type ('mixture,'id) init_t =
  | INIT_MIX of 'mixture
  | INIT_TOK of 'id

type ('mixture,'id) init_statment =
  string Locality.annot option *
  ('mixture,'id) Alg_expr.e Locality.annot *
  ('mixture,'id) init_t Locality.annot

type ('mixture,'id) instruction =
  | SIG      of agent
  | TOKENSIG of string Locality.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of ('mixture,'id) init_statment
  | DECLARE  of ('mixture,'id) variable_def
  | OBS      of ('mixture,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('mixture,'id) Alg_expr.e Locality.annot
  | PERT     of ('mixture,'id) perturbation
  | CONFIG   of configuration

type ('mixture,'id) command =
  | RUN of ('mixture,'id) Alg_expr.bool Locality.annot
  | MODIFY of ('mixture,'id) modif_expr list
  | QUIT

type ('agent,'mixture,'id,'rule,'edit_rule) compil =
  {
    variables :
      ('mixture,'id) variable_def list;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures :
      'agent list; (**agent signature declaration*)
    rules :
      (string Locality.annot option * 'rule Locality.annot) list;
    (**rules (possibly named)*)
    edit_rules : (string Locality.annot option * 'edit_rule) list;
    (** rules with explicit modifications*)
    observables :
      ('mixture,'id) Alg_expr.e Locality.annot list;
    (*list of patterns to plot*)
    init : ('mixture,'id) init_statment list;
    (*initial graph declaration*)
    perturbations :
      ('mixture,'id) perturbation list;
    configurations :
      configuration list;
    tokens :
      string Locality.annot list;
    volumes :
      (string * float * string) list
  }

type parsing_compil = (agent,mixture,string,rule,edit_rule) compil

val empty_compil : parsing_compil

val no_more_site_on_right : bool -> site list -> site list -> bool

val split_mixture : mixture -> (mixture * mixture * mixture * mixture)
(** @return [lhs,rhs,add,del] *)

val implicit_signature : parsing_compil -> parsing_compil
(** Infer agent signatures and tokens from init, rules and perturbations *)

(** {6 Printers} *)

val print_link :
  new_syntax:bool ->
  ('a -> Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) link -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_rule : Format.formatter -> rule -> unit
val print_ast_edit_rule : Format.formatter -> edit_rule -> unit
val print_ast_rule_no_rate :
  reverse:bool -> Format.formatter -> rule -> unit

val link_to_json :
  ('a -> 'a -> Yojson.Basic.json) -> ('a -> Yojson.Basic.json) ->
  ('b -> Yojson.Basic.json list) -> ('a, 'b) link -> Yojson.Basic.json
(** Fragile: the list MUST NOT be a singleton *)

val link_of_json :
  ('a -> Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'a) ->
  (Yojson.Basic.json list -> 'b) -> Yojson.Basic.json -> ('a, 'b) link

val compil_of_json : Yojson.Basic.json -> parsing_compil
val compil_to_json : parsing_compil -> Yojson.Basic.json

val compile_counters : parsing_compil -> parsing_compil * bool
val empty_counter : counter
