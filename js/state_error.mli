(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(*
val clear_errors : unit -> unit
(** Clear errors. *)

val set_errors : append:bool -> string -> Api_types_j.errors -> unit
(** Set errors

    Called with the location macro:
    set_error  __LOC__ msg
    The location and message will be logged
    debugging is enabled.

    @param location of error the macro __LOC__ is expected.
    @paramer errors to be saved
 *)
*)

val has_errors : unit -> bool
(** Return true if errors are present. *)

val errors : Api_types_j.errors option React.signal
(** Signal containing the error. *)

val wrap : ?append:bool -> string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t
(** This displays the error *)
