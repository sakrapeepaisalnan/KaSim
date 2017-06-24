(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Collection of rectangular instances. *)
(** Two implementations are available for this signature. *)

module type S = sig

type t

type mod_ccs_cache = (int, unit) Hashtbl.t

type message

val send_message : message -> t -> t

val empty : Model.t -> t

val debug_print : Format.formatter -> t -> unit


(** {6 Updating the roots} *)

val incorporate_extra_pattern : t -> Pattern.id -> IntCollection.t -> unit

val break_apart_cc :
  t -> Edges.t -> mod_ccs_cache -> (int * int) option -> t

val merge_cc : t -> mod_ccs_cache -> (int * int) option -> t

val update_roots :
  t -> bool -> Pattern.Set.t -> Edges.t ->
  mod_ccs_cache -> Pattern.Set.elt -> int -> unit


(** {6 Counting instances} *)

val number_of_instances : t -> Pattern.id array -> int
(** [number_of_instances state patterns] *)

val number_of_unary_instances_in_cc : 
  t -> Pattern.id * Pattern.id -> int -> int
(** [number_of_unary_instances_in_cc state (pat1, pat2) cc] *)


(** {6 Picking instances} *)

val pick_unary_instance_in_cc :
  t -> Random.State.t -> Pattern.id * Pattern.id -> int -> int * int
(** [pick_unary_instance_in_cc state random_state (pat1, pat2) cc]
    Returns a pair of roots corresponding to [pat1] and [pat2] respectively. 
    Optimized for currying before the [cc] argument. 
    In case of failure, one of the resulting roots is set to [(-1)]. *)

val fold_picked_instance :
  t -> Random.State.t -> Pattern.id array ->
  init:'a -> (int -> Pattern.id -> int -> 'a -> 'a option) -> 'a option
(** [fold_picked_instances state random_state patterns ~init f]
    with [f pat_id_in_array pat corresponding_root acc].
    Monadic fold function that calls [f] for every root of a random
    embedding from [patterns] in the mixture.
    This function is lazy in the sense it stops to draw roots when
    the accumulator besomes `None`. *)


(** {6 Enumerating instances} *)

val fold_instances :
  ?excp:(Pattern.id * int) ->
  t -> Pattern.id array -> init:'a -> (int list -> 'a -> 'a) -> 'a
(** [fold_enumerated_instances state patterns ~init f]
    with [f roots acc].
    Folds through every rectangular instance of an array of patterns. *)

val fold_unary_instances :
  t -> Pattern.id * Pattern.id -> init:'a -> (int * int -> 'a -> 'a) -> 'a
(** [fold_unary_instances state (pat1, pat2) ~init f ]
    with [f (root1, root2) acc]. *)

end