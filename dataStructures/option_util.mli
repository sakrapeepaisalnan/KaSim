(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Utilities on option *)

val map : ('a -> 'b) -> 'a option -> 'b option
val bind : ('a -> 'b option) -> 'a option -> 'b option
val unsome : 'a -> 'a option -> 'a
val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
