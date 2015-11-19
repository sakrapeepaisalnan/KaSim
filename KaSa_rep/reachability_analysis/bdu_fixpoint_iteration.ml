(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 9th of October
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open SetMap
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build_common
open Bdu_structure
open Fifo
open Printf

(************************************************************************************)
(*fixpoint iteration function*)
(*from an array of rule, build bdu:{of creation rule; test; list of modification}.
  the idea of iterate function: 
  - if working list is empty then return the bdu_remanent_array;
  - pop the first element inside the working list;
  - get 'rule' type from the rule_array;
  - build bdu_creation; bdu_test, and a list of action;
  - build a list of type List_sig.list for modif_list (list of action);
  - check if it  is an enable rule or not:
    it is an enable rule if the result of intersection of test and init is not empty;
    + if it is an enable rule return the result.
    + if it is not an enable rule: call iterate update function:
      ++ bdu_creation intersection (mvbdu_and) with bdu_test, 
         then use the result above: redefine (redefine function in mvbdu) it
         with the list of modif;
      ++ do the union (mvbdu_or) of bdu_assignment (the intersection above) and bdu_creation;
      ++ store this bdu_update in bdu_array, with the index is rule_id.
*)

(************************************************************************************)
(*is enable rule*)

let is_belong bdu bdu_init =
  let is_eq = Mvbdu_sanity.safety_equal_mvbdu bdu bdu_init in
  if is_eq
  then true
  else false
    
let comp_is_enable parameter error handler bdu_init bdu_test bdu_creation =
  let error, handler, bdu_X =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_creation
  in
  if not (is_belong bdu_X bdu_init)
  then
    error, true
  else
    error, false

(************************************************************************************)
(*update bdu*)

let compute_update parameter error handler bdu_test list_a bdu_X =
  (*intersection of X and bdu_test*)
  let error, handler, bdu_inter_test_X =
    f parameter error bdu_test
      (boolean_mvbdu_and parameter handler error parameter bdu_test) bdu_X
  in
  (*redefine with a list of modification*)
  let error, handler, bdu_assigment =
    f parameter error bdu_inter_test_X
      (redefine parameter error parameter handler bdu_inter_test_X) list_a
  in
  (*union with bdu_X*)
  let error, handler, bdu_update =
    f parameter error bdu_assigment
      (boolean_mvbdu_or parameter handler error parameter bdu_assigment) bdu_X
  in
  error, bdu_update

(************************************************************************************)
(*fixpoint*)

let collect_bdu_update_map parameter error rule wl_creation 
    store_remanent_test
    store_test_bdu_map
    store_remanent_creation
    store_creation_bdu_map
    store_result
    =
  let error, (handler, bdu_init) = bdu_init parameter error in
  let add_link (agent_type, cv_id) bdu_update store_result =
    let (l, old) =
      Map_bdu_update.Map.find_default ([], bdu_init) (agent_type, cv_id) store_result
    in
    let result_map = (*FIXME: list or bdu_init ? *)
      Map_bdu_update.Map.add (agent_type, cv_id) (l, bdu_update) 
        store_result
    in
    error, result_map
  in  
  (*iterate function over working list*)
  let rec aux acc_wl (error, bdu_update_map) =
    if IntWL.is_empty acc_wl
    then
      error, bdu_update_map
    else
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) =
        IntWL.pop parameter error acc_wl
      in
      match rule_id_op with
      | None -> error, bdu_update_map
      | Some rule_id ->
        (*--------------------------------------------------------------------*)
        (*get the local view that is tested for this rule_id with new indexes*)
        let error, bdu_test =
          (*take a global view *)
          AgentMap.fold parameter error
            (fun parameter error agent_id agent store_bdu_result ->
              match agent with
              | Ghost -> error, store_bdu_result (*if it is a ghost agent?*)
              | Agent agent ->
                let agent_type = agent.agent_name in
                (*get the local view: (agent_id, rule_id, cv_id, pair_list)list *)
                let error, get_test_list =
                  match 
                    AgentMap.unsafe_get parameter error agent_type store_remanent_test 
                  with
                  | error, None -> error, []
                  | error, Some l -> error, l
                in
                (*match the rule_id, if yes, then return the bdu_test,if
                  not continue to the rest of the list*)
                let error, bdu_test =
                  List.fold_left (fun (error, bdu_result) (agent_id, rule_id', cv_id, _) ->
                    if rule_id = rule_id'
                    then
                      (*return the bdu_test*)
                      let (l, bdu_test) =
                        Map_test_bdu.Map.find_default ([], bdu_init)
                          (agent_id, agent_type, rule_id, cv_id) store_test_bdu_map
                      in
                      error, bdu_test
                    else
                    (*return the result*)
                      error, bdu_result
                  ) (error, store_bdu_result) get_test_list
                in
                error, bdu_test
            ) rule.rule_lhs.views bdu_init (*TODO*)
        in
        (*--------------------------------------------------------------------*)
        (*get the local view that is created for this rule_id with new_indexes*)
        let error, bdu_creation =
          (*take a global view*)
          List.fold_left (fun (error, store_bdu_result) (agent_id, agent_type) ->
            let error, agent = AgentMap.get parameter error agent_id rule.rule_rhs.views in
            match agent with
            | None -> warn parameter error (Some "line 163") Exit store_bdu_result
            | Some Ghost -> error, store_bdu_result
            | Some Agent agent ->
              (*get the local view (rule_id, cv_id, pair_list) list*)
              let error, get_creation_list =
                match
                  AgentMap.unsafe_get parameter error agent_type store_remanent_creation
                with
                | error, None -> error, []
                | error, Some l -> error, l

              in
              (*match rule_id with rule_id', if yes then creation
                bdu_creation, if not continue to the rest of this list*)
              let error, bdu_creation =
                List.fold_left (fun (error, store_result) (rule_id', cv_id, _) ->
                  if rule_id = rule_id'
                  then 
                    let (l, bdu_creation) =
                      Map_creation_bdu.Map.find_default ([], bdu_init)
                        (agent_type, rule_id, cv_id) store_creation_bdu_map
                    in
                    error, bdu_creation
                  else
                    error, store_result
                ) (error, store_bdu_result) get_creation_list
              in
              error, bdu_creation
          ) (error, bdu_update_map) (*bdu_init*) rule.actions.creation
        in
        (*--------------------------------------------------------------------*)
        (*TODO:continue to the rest of this working list*)
        aux wl_tl (error, bdu_creation)
  in
  aux wl_creation (error, store_result)
  



(*let collect_bdu_update_array parameter error
    wl_creation 
    store_bdu_creation_map
    store_bdu_test_map
    store_modif_list_map
    =
  let add_link (agent_type, rule_id) bdu_remanent store_result =
    let (l, old) =
      Map_bdu_update.Map.find_default ([], []) (agent_type, rule_id) store_result
    in
    let result_map =
      Map_bdu_update.Map.add (agent_type, rule_id) (l, bdu_remanent :: []) 
        store_result
    in
    error, result_map
  in
  let error, (handler, bdu_init) = bdu_init parameter error in
  (*iterate function over working list and the result of bdu_update*)
  let rec aux acc_wl (error, bdu_update_map) =
    (*if there is no working list*)
    if IntWL.is_empty acc_wl
    then 
      error, bdu_update_map
    else
      (*pop the first elemenent (rule_id) in this working list out*)
      let error, (rule_id_op, wl_tl) =
        IntWL.pop parameter error acc_wl
      in
      match rule_id_op with
      | None -> error, bdu_update_map
      | Some rule_id ->
        (*get bdu_creation from this rule_id inside the bdu_creation_map*)
        if Map_creation_bdu.Map.is_empty store_bdu_creation_map &&
          Map_test_bdu.Map.is_empty store_bdu_test_map &&
          Map_modif_list.Map.is_empty store_modif_list_map
        then error, bdu_update_map (*waring*)
        else
          begin
            Map_creation_bdu.Map.fold (fun (agent_type, rule_id_creation)
              (l1, bdu_creation) (error, store_result_map1) ->
                Map_test_bdu.Map.fold (fun (agent_type_test, rule_id_test)
                  (l2, bdu_test) (error, store_result_map2) ->
                    Map_modif_list.Map.fold (fun (agent_type_modif, rule_id_modif)
                      (l3, list_a) (error, store_result_map3) ->
                        if rule_id = rule_id_creation
                        then
                          begin
                            (*build a list of type List_sig.list for modif_list*)
                            (*check the enable rule*)
                            let error, is_enable =
                              comp_is_enable
                                parameter
                                error
                                handler
                                bdu_init
                                bdu_test
                                bdu_creation
                            in
                            (*is not enable*)
                            if is_enable
                            then
                              (*update bdu*)
                              let error, bdu_update =
                                compute_update
                                  parameter
                                  error
                                  handler
                                  bdu_test
                                  list_a
                                  bdu_creation                            
                              in
                              let error, result_map =
                                add_link (agent_type, rule_id) bdu_update store_result_map3
                              in
                              aux wl_tl (error, result_map)
                            else
                              (*continue with old_result*)
                              aux wl_tl (error, store_result_map3)                          
                          end
                        else
                          aux wl_tl (error, store_result_map1)
                    ) store_modif_list_map (error, store_result_map2) (*FIXME*)
                ) store_bdu_test_map (error, store_result_map1)
            ) store_bdu_creation_map (error, bdu_update_map)
          end
  in
  aux wl_creation (error, Map_bdu_update.Map.empty)*)
