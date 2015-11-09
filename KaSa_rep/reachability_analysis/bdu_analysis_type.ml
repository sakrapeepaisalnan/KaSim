(**
  * bdu_analysis_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 15th of July
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Int_storage
open Fifo
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

(************************************************************************************)
(*module type*)

module AgentMap = Quick_Nearly_inf_Imperatif

(*module of covering classes*)

module Int2Map_CV =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*module type of contact map with state*)

module Int2Map_CM_state =
  SetMap.Make (
    struct
      (*agent_type, site, state*)
      type t = int * int * int
      let compare = compare
    end
  )

(*------------------------------------------------------------------------------*)
(*module type of modification site*)

module Int2Map_Modif =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*half break side effect module*)

module Int2Map_HalfBreak_effect =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

module Int2Map_Remove_effect =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*update function of covering classes and modification sites*)

module Int2Map_CV_Modif =
  SetMap.Make (
    struct
      (*agent_type, site, covering_class_id*)
      type t = int * int * int
      let compare = compare
    end)

(************************************************************************************)
(*module type for bdu structure*)

module Map_test =
  SetMap.Make (
    struct
      (*agent_type, cv_id, site, state, rule_id*)
      type t = int * int * int * int
      let compare = compare
    end)

module Map_creation =
  SetMap.Make (
    struct
      (*agent_type, cv_id, site, state, rule_id*)
      type t = int * int * int * int
      let compare = compare
    end)

module Map_modif =
  SetMap.Make (
    struct
      (*agent_type, cv_id, site, state, rule_id*)
      type t = int * int * int * int
      let compare = compare
    end)

module Map_modif_creation = (*without creation rules*)
  SetMap.Make (
    struct
      (*agent_type, cv_id, site, state, rule_id*)
      type t = int * int * int * int
      let compare = compare
    end)

(************************************************************************************)
(*static information*)

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.Map.t

(*do not consider the case where site has state free.*)
type remove_action =
  (int list * int list) Int2Map_Remove_effect.Map.t

type bdu_analysis_static =
  {
    store_covering_classes_id : (int list * int list) Int2Map_CV.Map.t;
    store_side_effects        : half_break_action * remove_action;
    store_creation_sites      : (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_modification_sites_without_creation : (int list * Site_map_and_set.Set.t)
      Int2Map_Modif.Map.t;
    store_modification_sites  :
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_test_sites :
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_test_modification_sites : 
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_test_modification_without_creation : (*use in update*)
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
  }

(************************************************************************************)
(*dynamic*)

type wl_int = IntWL.WSetMap.elt list * IntWL.WSetMap.elt list * IntWL.WSetMap.Set.t

type bdu_analysis_dynamic =
  {
    store_contact_map      : 
    (*TODO: combine contact map and modification update into a product type*)
    (int list * (int * int * int) list) Int2Map_CM_state.Map.t;
    store_covering_classes_modification_update :
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
    store_update :
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t *
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t *
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t *
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
    store_wl_update          : wl_int AgentMap.t;
    store_wl_creation        : wl_int AgentMap.t;
    store_wl_creation_update : wl_int AgentMap.t
  }

(************************************************************************************)
(*build covering classes with new index*)

type pair_bdu =
  (Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
   Boolean_mvbdu.list_dic, bool, int) Memo_sig.handler * bool Mvbdu_sig.mvbdu
  
type bdu_build =
  {
    store_remanent_triple   : ((int * int list * Site_map_and_set.Set.t) list) AgentMap.t;
    store_remanent_test     : (int * int * int * int) list AgentMap.t;
    store_remanent_creation : (int * int * int * int) list AgentMap.t;
    store_remanent_modif    : (int * int * int * int) list AgentMap.t;
  }

(************************************************************************************)
(*build covering classes in map and bdu with new indexes for site_type*)

type pair_map = int list * Site_map_and_set.Set.t

type bdu_build_map =
  {
    store_remanent_test_map     : pair_map Map_test.Map.t;
    store_remanent_creation_map : pair_map Map_creation.Map.t;
    store_remanent_modif_map    : pair_map Map_modif.Map.t;
    store_remanent_modif_op_map : pair_map Map_modif_creation.Map.t;
  }

(************************************************************************************)
(*main*)

type bdu_analysic =
    {
      store_bdu_analysis_static  : bdu_analysis_static;
      store_bdu_analysis_dynamic : bdu_analysis_dynamic;
      store_bdu_build            : bdu_build;
      store_bdu_build_map        : bdu_build_map;
    }
