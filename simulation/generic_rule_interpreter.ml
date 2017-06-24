(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type precomputed =
  {
    unary_patterns: Pattern.Set.t;
    always_outdated: Operator.DepSet.t;
  }

module Make (Instances:Instances_sig.S) = struct
  type t =
    {
      mutable outdated : bool;

      precomputed: precomputed;
      instances: Instances.t;

      (* Without rectangular approximation *)
      matchings_of_rule:
        (Matching.t * int list) list Mods.IntMap.t;
      unary_candidates:
       (Matching.t * Edges.path option) list Mods.IntMap.t;

      variables_cache: Nbr.t array;
      variables_overwrite: Alg_expr.t option array;

      edges: Edges.t;
      tokens: Nbr.t array;
      outdated_elements: Operator.DepSet.t * (int,unit) Hashtbl.t;

      activities : Random_tree.tree;
      (* pair numbers are regular rule, odd unary instances *)

      random_state : Random.State.t;

      story_machinery :
        (string (*obs name*) * Pattern.id array *
         Instantiation.abstract Instantiation.test list list)
          list Pattern.ObsMap.t (*currently tracked ccs *) option;
      species :
        (string (*filename*) * Pattern.id array (*with only one pattern*) *
         Instantiation.abstract Instantiation.test list list) list
          Pattern.ObsMap.t;
    }

  (* Utilities to deal with Instances.t *)

  let instances_sum_numbers insts l = 
    List.map (Instances.number insts) l
    |> List.fold_left (+) 0

  type result = Clash | Corrected | Success of t

  let raw_get_alg env overwr i =
    match overwr.(i) with
    | None -> Model.get_alg env i
    | Some expr -> expr

  let value_bool counter state expr =
    let () = assert (not state.outdated) in
    Expr_interpreter.value_bool
      counter ~get_alg:(fun i -> Alg_expr.CONST state.variables_cache.(i))
      ~get_mix:(fun patterns -> Nbr.I (instances_sum_numbers state.instances patterns))
      ~get_tok:(fun i -> state.tokens.(i))
      expr
  let value_alg counter state alg =
    let () = assert (not state.outdated) in
    Expr_interpreter.value_alg
      counter ~get_alg:(fun i -> Alg_expr.CONST state.variables_cache.(i))
      ~get_mix:(fun patterns -> Nbr.I (instances_sum_numbers state.instances patterns))
      ~get_tok:(fun i -> state.tokens.(i))
      alg


  let recompute env counter state i =
    state.variables_cache.(i) <-
      value_alg counter state (raw_get_alg env state.variables_overwrite i)


  let activity state = Random_tree.total state.activities
  let get_activity rule state = Random_tree.find rule state.activities
  let set_activity rule v state = Random_tree.add rule v state.activities
  let pick_rule rt state = fst (Random_tree.random rt state.activities)

  let initial_activity env counter state =
    Model.fold_rules
      (fun i () rule ->
         if Array.length rule.Primitives.connected_components = 0 then
           match Nbr.to_float @@ value_alg
               counter state (fst rule.Primitives.rate) with
           | None ->
             ExceptionDefn.warning ~pos:(snd rule.Primitives.rate)
               (fun f -> Format.fprintf f "Problematic rule rate replaced by 0")
           | Some rate -> set_activity (2*i) rate state)
      () env

  let empty ~with_trace random_state env counter =
    let activity_tree = Random_tree.create (2*Model.nb_rules env) in
    let unary_patterns =
      Model.fold_rules
        (fun _ acc r ->
           match r.Primitives.unary_rate with
           | None -> acc
           | Some _ ->
             Pattern.Set.add
               r.Primitives.connected_components.(0)
               (Pattern.Set.add r.Primitives.connected_components.(1) acc)
        ) Pattern.Set.empty env in
    let always_outdated =
      let (deps_in_t,deps_in_e,_,_) = Model.all_dependencies env in
      Operator.DepSet.union deps_in_t deps_in_e in
    let with_connected_components = not (Pattern.Set.is_empty unary_patterns) in
    let variables_overwrite = Array.make (Model.nb_algs env) None in
    let variables_cache = Array.make (Model.nb_algs env) Nbr.zero in
    let cand =
      {
        activities = activity_tree ;
        outdated = false;
        precomputed = { unary_patterns; always_outdated};
        instances = Instances.empty env;
        matchings_of_rule = Mods.IntMap.empty;
        unary_candidates = Mods.IntMap.empty;
        variables_overwrite; variables_cache;
        edges = Edges.empty ~with_connected_components;
        tokens = Array.make (Model.nb_tokens env) Nbr.zero;
        outdated_elements = always_outdated,Hashtbl.create 32;
        random_state;
        story_machinery =
          if with_trace then
            Some (Pattern.Env.new_obs_map
                    (Model.domain env) (fun _ -> []))
          else None;
        species = Pattern.Env.new_obs_map
            (Model.domain env) (fun _ -> []);
      } in
    let () = Tools.iteri (recompute env counter cand) (Model.nb_algs env) in
    let () = initial_activity env counter cand in
    cand


  (* @Jonathan: reimported from Instances *)

  let pop_exact_matchings state rule =
    match Mods.IntMap.pop rule state.matchings_of_rule with
    | None,_ -> state
    | Some _, match' -> { state with matchings_of_rule = match' }

  let pick_a_rule_instance state random_state domain edges ?rule_id rule =
    let from_patterns () =
        Instances.pick_an_instance state.instances random_state domain edges
        rule.Primitives.connected_components in
    match rule_id with
    | None -> from_patterns ()
    | Some id ->
      match Mods.IntMap.find_option id state.matchings_of_rule with
      | Some [] -> None
      | Some l -> Some (List_util.random random_state l)
      | None -> from_patterns ()

  let adjust_rule_instances ~rule_id ?unary_rate state domain edges ccs =
    let matches = Instances.all_injections state.instances ?unary_rate domain edges ccs in
    List.length matches,
    { state with
      matchings_of_rule =
        Mods.IntMap.add rule_id matches state.matchings_of_rule }

  let compute_unary_number state modified_cc rule cc =
    let va, instances = Instances.compute_unary_number state.instances modified_cc rule cc in
    let state = 
      match Mods.IntMap.pop cc state.unary_candidates with
      | None, _ -> { state with instances }
      | Some _, unary_candidates -> { state with instances ; unary_candidates } in
    (va, state)




    
    
  let pick_a_unary_rule_instance state random_state domain edges ~rule_id rule =
    match Mods.IntMap.find_option rule_id state.unary_candidates with
    | Some l ->
       let inj,path = List_util.random random_state l in 
       Some inj,path
    | None -> 
      Instances.pick_a_unary_instance state.instances 
        random_state domain edges ~rule_id rule


  let adjust_unary_rule_instances ~rule_id ?max_distance state domain graph pats =
    let a, unary_candidates = 
      Instances.update_unary_candidates ~rule_id ?max_distance 
        state.instances domain graph pats state.unary_candidates in
    a, { state with unary_candidates }




  let print env f state =
    let sigs = Model.signatures env in
    Format.fprintf
      f "@[<v>%a@,%a@]"
      (Pp.list Pp.space (fun f (i,mix) ->
           Format.fprintf f "%%init: %i @[<h>%a@]" i
             (Raw_mixture.print ~new_syntax:false ~compact:false ~created:false ~sigs)
             mix))
      (Edges.build_snapshot sigs state.edges)
      (Pp.array Pp.space (fun i f el ->
           Format.fprintf
             f "%%init: %a <- %a"
             (Model.print_token ~env) i Nbr.print el))
      state.tokens

  let debug_print f state =
    Format.fprintf
      f "@[<v>%a@,%a@,%a@]"
      Edges.debug_print state.edges
      (Pp.array Pp.space (fun i f el ->
           Format.fprintf f "token_%i <- %a"
             i Nbr.print el))
      state.tokens
      Instances.debug_print state.instances

  type stats = { mixture_stats : Edges.stats }

  let stats state = {
    mixture_stats = Edges.stats state.edges;
  }

  let print_stats f state =
    Format.fprintf f "%i agents" (stats state).mixture_stats.Edges.nb_agents

  let new_place free_id (inj_nodes,inj_fresh) = function
    | Matching.Agent.Existing _ -> failwith "Rule_interpreter.new_place"
    | Matching.Agent.Fresh (_,id) ->
      (inj_nodes,Mods.IntMap.add id free_id inj_fresh)

  let apply_negative_transformation
      mod_connectivity (side_effects,stuff4unaries,edges) = function
    | Primitives.Transformation.Agent (id,_) ->
      let edges' = Edges.remove_agent id edges in
      (side_effects,stuff4unaries,edges')
    | Primitives.Transformation.Freed ((id,_),s) -> (*(n,s)-bottom*)
      let edges' = Edges.remove_free id s edges in
      (side_effects,stuff4unaries,edges')
    | Primitives.Transformation.Linked (((id,_),s),((id',_),s')) ->
      let edges',cc_modif = Edges.remove_link id s id' s' edges in
      (side_effects,
       Instances.break_apart_cc stuff4unaries edges' mod_connectivity cc_modif,edges')
    | Primitives.Transformation.NegativeWhatEver ((id,_),s as n) ->
      begin
        match (List.partition (fun x -> x =n) side_effects) with
        | (_::_,side_effects') -> (side_effects',stuff4unaries,edges)
        | ([],_) ->
          match Edges.link_destination id s edges with
          | None -> (side_effects,stuff4unaries,Edges.remove_free id s edges)
          | Some ((id',_ as nc'),s') ->
            let edges',cc_modif = Edges.remove_link id s id' s' edges in
            ((nc',s')::side_effects,
             Instances.break_apart_cc stuff4unaries edges' mod_connectivity cc_modif,
             edges')
      end
    | Primitives.Transformation.PositiveInternalized _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot "PositiveInternalized in negative update"))
    | Primitives.Transformation.NegativeInternalized ((id,_),s) ->
      let _,edges' = Edges.remove_internal id s edges in
      (side_effects,stuff4unaries,edges')

  let apply_positive_transformation
      sigs mod_connectivity (inj2graph,side_effects,stuff4unaries,edges) = function
    | Primitives.Transformation.Agent n ->
      let nc, inj2graph',edges' =
        let ty = Matching.Agent.get_type n in
        let id,edges' = Edges.add_agent sigs ty edges in
        (id,ty),new_place id inj2graph n,edges' in
      (inj2graph',side_effects,stuff4unaries,edges'),
      Primitives.Transformation.Agent nc
    | Primitives.Transformation.Freed (n,s) -> (*(n,s)-bottom*)
      let (id,_ as nc) = Matching.Agent.concretize inj2graph n in (*(A,23)*)
      let edges' = Edges.add_free id s edges in
      let side_effects' =
        List_util.smart_filter (fun x -> x <> (nc,s)) side_effects in
      (inj2graph,side_effects',stuff4unaries,edges'),
      Primitives.Transformation.Freed (nc,s)
    | Primitives.Transformation.Linked ((n,s),(n',s')) ->
      let nc = Matching.Agent.concretize inj2graph n in
      let nc' = Matching.Agent.concretize inj2graph n' in
      let edges',modif_cc = Edges.add_link nc s nc' s' edges in
      let side_effects' = List_util.smart_filter
          (fun x -> x<>(nc,s) && x<>(nc',s')) side_effects in
      (inj2graph,side_effects',
       Instances.merge_cc stuff4unaries mod_connectivity modif_cc,edges'),
      Primitives.Transformation.Linked ((nc,s),(nc',s'))
    | Primitives.Transformation.NegativeWhatEver _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot "NegativeWhatEver in positive update"))
    | Primitives.Transformation.PositiveInternalized (n,s,i) ->
      let (id,_ as nc) = Matching.Agent.concretize inj2graph n in
      let edges' = Edges.add_internal id s i edges in
      (inj2graph,side_effects,stuff4unaries,edges'),
      Primitives.Transformation.PositiveInternalized (nc,s,i)
    | Primitives.Transformation.NegativeInternalized _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot "NegativeInternalized in positive update"))

  let apply_concrete_positive_transformation
      sigs mod_connectivity (stuff4unaries,edges) = function
    | Primitives.Transformation.Agent (id,ty) ->
      let _,edges' = Edges.add_agent ~id sigs ty edges in
      (stuff4unaries,edges')
    | Primitives.Transformation.Freed ((id,_),s) -> (*(n,s)-bottom*)
      let edges' = Edges.add_free id s edges in
      (stuff4unaries,edges')
    | Primitives.Transformation.Linked ((nc,s),(nc',s')) ->
      let edges',modif_cc = Edges.add_link nc s nc' s' edges in
      (Instances.merge_cc stuff4unaries mod_connectivity modif_cc,edges')
    | Primitives.Transformation.NegativeWhatEver _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot "NegativeWhatEver in positive update"))
    | Primitives.Transformation.PositiveInternalized ((id,_),s,i) ->
      let edges' = Edges.add_internal id s i edges in
      (stuff4unaries,edges')
    | Primitives.Transformation.NegativeInternalized _ ->
      raise
        (ExceptionDefn.Internal_Error
           (Locality.dummy_annot "NegativeInternalized in positive update"))

  let obs_from_transformation domain edges acc = function
    | Primitives.Transformation.Agent nc ->
      Matching.observables_from_agent domain edges acc nc
    | Primitives.Transformation.Freed (nc,s) -> (*(n,s)-bottom*)
      Matching.observables_from_free domain edges acc nc s
    | Primitives.Transformation.Linked ((nc,s),(nc',s')) ->
      Matching.observables_from_link
        domain edges acc nc s nc' s'
    | Primitives.Transformation.PositiveInternalized (nc,s,i) ->
      Matching.observables_from_internal
        domain edges acc nc s i
    | Primitives.Transformation.NegativeInternalized ((id,_ as nc),s) ->
      let i  = Edges.get_internal id s edges in
      Matching.observables_from_internal
        domain edges acc nc s i
    | Primitives.Transformation.NegativeWhatEver ((id,_ as nc),s) ->
      match Edges.link_destination id s edges with
      | None ->
        Matching.observables_from_free domain edges acc nc s
      | Some (nc',s') ->
        Matching.observables_from_link
          domain edges acc nc s nc' s'

  let path_tests path tests =
    let known_agents =
      List.fold_left
        (List.fold_left
           (fun acc -> function
              | Instantiation.Is_Here (id,_) -> Mods.IntSet.add id acc
              | Instantiation.Is_Bound_to _ | Instantiation.Is_Bound _
              | Instantiation.Has_Internal _ | Instantiation.Has_Binding_type _
              | Instantiation.Is_Free _ -> acc)) Mods.IntSet.empty tests in
    let pretests =
      List_util.map_option
        (fun (x,y) ->
           if List.for_all
               (List.for_all
                  (function
                    | Instantiation.Is_Bound_to (a,b) ->
                      x <> a && x <> b && y<>a && y<>b
                    | Instantiation.Has_Internal _ | Instantiation.Is_Free _
                    | Instantiation.Is_Bound _ | Instantiation.Has_Binding_type _
                    | Instantiation.Is_Here _ -> true)) tests
           then Some (Instantiation.Is_Bound_to (x,y))
           else None) path in
    let _,path_tests =
      List.fold_left
        (fun (ag,te) (((id,_ as a),_),((id',_ as a'),_)) ->
           let ag',te' =
             if Mods.IntSet.mem id ag then ag,te
             else Mods.IntSet.add id ag,Instantiation.Is_Here a::te in
           if Mods.IntSet.mem id' ag' then ag',te'
           else Mods.IntSet.add id' ag',Instantiation.Is_Here a'::te')
        (known_agents,pretests) path in
    path_tests

  let step_of_event counter = function
    | Trace.INIT _,e -> (Trace.Init e.Instantiation.actions)
    | Trace.RULE r,x ->
      (Trace.Rule (r,x,Counter.current_simulation_info counter))
    | Trace.PERT p,x ->
      (Trace.Pert (p,x,Counter.current_simulation_info counter))

  let store_event counter inj2graph new_tracked_obs_instances event_kind
      ?path extra_side_effects rule outputs = function
    | None -> ()
    | Some _ ->
      let cevent =
        Instantiation.concretize_event inj2graph rule.Primitives.instantiations in
      let full_concrete_event = {
        Instantiation.tests = cevent.Instantiation.tests;
        Instantiation.actions = cevent.Instantiation.actions;
        Instantiation.side_effects_src = cevent.Instantiation.side_effects_src;
        Instantiation.side_effects_dst = List.rev_append
            extra_side_effects cevent.Instantiation.side_effects_dst;
        Instantiation.connectivity_tests =
          (match path with
           | None -> []
           | Some path -> path_tests path cevent.Instantiation.tests);
      } in
      let () =
        outputs (Data.TraceStep
                   (step_of_event counter (event_kind,full_concrete_event))) in
      List.iter
        (fun (i,x) ->
           outputs (Data.TraceStep
                      (Trace.Obs(i,x,Counter.next_story counter))))
        new_tracked_obs_instances

  let get_species_obs sigs edges obs acc tracked =
    List.fold_left
      (fun acc (pattern,(root,_)) ->
         try
           List.fold_left
             (fun acc (fn,patterns,_) ->
                if Array.fold_left
                    (fun ok pid ->
                       ((Pattern.compare_canonicals pid pattern) = 0)||ok)
                    false patterns
                then
                  let spec = Edges.species sigs root edges in
                  (fn,patterns,spec)::acc else acc)
             acc (Pattern.ObsMap.get tracked pattern)
         with Not_found -> acc)
      acc obs

  let store_obs domain edges instances obs acc = function
    | None -> acc
    | Some tracked ->
      List.fold_left
        (fun acc (pattern,(root,_)) ->
           try
             List.fold_left
               (fun acc (ev,patterns,tests) ->
                  List.fold_left
                    (fun acc (inj,_) ->
                       let tests' =
                         List.map
                           (List.map (Instantiation.concretize_test
                                        (inj,Mods.IntMap.empty))) tests in
                       (ev,tests') :: acc)
                    acc
                    (Instances.all_injections
                       instances ~excp:(pattern,root) domain edges patterns))
               acc (Pattern.ObsMap.get tracked pattern)
           with Not_found -> acc)
        acc obs

  let update_edges
      outputs counter domain inj_nodes state event_kind ?path rule sigs =
    let () = assert (not state.outdated) in
    let () = state.outdated <- true in
    let former_deps,mod_connectivity = state.outdated_elements in
    (*Negative update*)
    let concrete_removed =
      List.map (Primitives.Transformation.concretize
                  (inj_nodes,Mods.IntMap.empty)) rule.Primitives.removed in
    let ((del_obs,del_deps),_) =
      List.fold_left
        (obs_from_transformation domain state.edges)
        (([],Operator.DepSet.empty),Matching.empty_cache)
        concrete_removed in
    let (side_effects,instances,edges_after_neg) =
      List.fold_left
        (apply_negative_transformation mod_connectivity)
        ([],(state.instances),state.edges)
        concrete_removed in
    let () =
      List.iter
        (fun (pat,(root,_)) ->
           Instances.update_roots instances false state.precomputed.unary_patterns
             edges_after_neg mod_connectivity pat root)
        del_obs in
    (*Positive update*)
    let (final_inj2graph,remaining_side_effects,instances',edges'),
        concrete_inserted =
      List.fold_left
        (fun (x,p) h ->
           let (x', h') =
             apply_positive_transformation
               (Pattern.Env.signatures domain) mod_connectivity x h in
           (x',h'::p))
        (((inj_nodes,Mods.IntMap.empty),side_effects,
          instances,edges_after_neg),[])
        rule.Primitives.inserted in
    let (edges'',concrete_inserted') =
      List.fold_left
        (fun (e,i)  ((id,_ as nc),s) ->
           Edges.add_free id s e,Primitives.Transformation.Freed (nc,s)::i)
        (edges',concrete_inserted) remaining_side_effects in
    let ((new_obs,new_deps),_) =
      List.fold_left
        (obs_from_transformation domain edges'')
        (([],Operator.DepSet.empty),Matching.empty_cache)
        concrete_inserted' in
    let () =
      List.iter
        (fun (pat,(root,_)) ->
           Instances.update_roots instances' true state.precomputed.unary_patterns
             edges'' mod_connectivity pat root)
        new_obs in
    (*Store event*)
    let new_tracked_obs_instances =
      store_obs domain edges'' instances' new_obs [] state.story_machinery in
    let () =
      store_event
        counter final_inj2graph new_tracked_obs_instances event_kind
        ?path remaining_side_effects rule outputs state.story_machinery in
    (*Print species*)
    let species =
      get_species_obs sigs edges'' new_obs [] state.species in
    let () =
      List.iter
        (fun (file,_,mixture) ->
           outputs (Data.Species
                      (file,(Counter.current_time counter),mixture))) species in
    let rev_deps = Operator.DepSet.union
        former_deps (Operator.DepSet.union del_deps new_deps) in
    { state with
      outdated = false;
      precomputed = state.precomputed;
      instances = instances';
      variables_cache = state.variables_cache;
      variables_overwrite = state.variables_overwrite;
      edges = edges''; tokens = state.tokens;
      outdated_elements = rev_deps,mod_connectivity;
      random_state = state.random_state;
      story_machinery = state.story_machinery;
      species = state.species;
    }

  let update_edges_from_actions
      ~outputs sigs counter domain state (actions,side_effect_dst) =
    let () = assert (not state.outdated) in
    let () = state.outdated <- true in
    let former_deps,mod_connectivity = state.outdated_elements in
    (*Negative update*)
    let concrete_removed =
      Primitives.Transformation.negative_transformations_of_actions
        sigs state.edges actions in
    let ((del_obs,del_deps),_) =
      List.fold_left
        (obs_from_transformation domain state.edges)
        (([],Operator.DepSet.empty),Matching.empty_cache)
        concrete_removed in
    let (_side_effects,instances,edges_after_neg) =
      List.fold_left
        (apply_negative_transformation mod_connectivity)
        ([],(state.instances),state.edges)
        concrete_removed in
    let () =
      List.iter
        (fun (pat,(root,_)) ->
           Instances.update_roots instances false state.precomputed.unary_patterns
             edges_after_neg mod_connectivity pat root)
        del_obs in
    (*Positive update*)
    let concrete_inserted =
      Primitives.Transformation.positive_transformations_of_actions
        sigs side_effect_dst actions in
    let (instances',edges') =
      List.fold_left
        (fun x h ->
           apply_concrete_positive_transformation
             (Pattern.Env.signatures domain) mod_connectivity x h)
        (instances,edges_after_neg)
        concrete_inserted in
    let ((new_obs,new_deps),_) =
      List.fold_left
        (obs_from_transformation domain edges')
        (([],Operator.DepSet.empty),Matching.empty_cache)
        concrete_inserted in
    let () =
      List.iter
        (fun (pat,(root,_)) ->
           Instances.update_roots instances' true state.precomputed.unary_patterns
             edges' mod_connectivity pat root)
        new_obs in
    (*Print species*)
    let species =
      get_species_obs sigs edges' new_obs [] state.species in
    let () =
      List.iter
        (fun (file,_,mixture) ->
           outputs (Data.Species
                      (file,(Counter.current_time counter),mixture))) species in
    let rev_deps = Operator.DepSet.union
        former_deps (Operator.DepSet.union del_deps new_deps) in
    { activities = state.activities ; 
      outdated = false;
      precomputed = state.precomputed;
      instances = instances';
      matchings_of_rule = state.matchings_of_rule ;
      unary_candidates = state.unary_candidates ;
      variables_cache = state.variables_cache;
      variables_overwrite = state.variables_overwrite;
      edges = edges'; tokens = state.tokens;
      outdated_elements = rev_deps,mod_connectivity;
      random_state = state.random_state;
      story_machinery = state.story_machinery;
      species = state.species;
    }

  let max_dist_to_int counter state d = Nbr.to_int (value_alg counter state d)

  (* cc_va is the number of embeddings. It only has 
  to be multiplied by the rate constant of the rule *)
  let store_activity store env counter state id syntax_id rate cc_va =
    let () =
      if !Parameter.debugModeOn then
        Format.printf "@[%sule %a has now %i instances.@]@."
          (if id mod 2 = 1 then "Unary r" else "R")
          (Model.print_rule ~env) (id/2) cc_va in
    let act =
      match Nbr.to_float @@ value_alg counter state rate with
      | None -> if cc_va = 0 then 0. else infinity
      | Some rate -> rate *. float_of_int cc_va in
    let () =
      if act < 0. then
        let unary = id mod 2 = 1 in
        raise
          (ExceptionDefn.Malformed_Decl
             ((Format.asprintf
                 "At t=%.2f %sctivity of rule %a has become negative (%f)"
                 (Counter.current_time counter)
                 (if unary then "Unary " else "")
                 (Model.print_rule ~env) id act),
              Model.get_ast_rule_rate_pos ~unary env syntax_id)) in
    let old_act = get_activity id state in
    let () = set_activity id act state in
    store syntax_id old_act act

  let update_outdated_activities store env counter state =
    let () = assert (not state.outdated) in
    let deps,changed_connectivity = state.outdated_elements in
    let unary_rule_update modified_cc i state rule =
      match rule.Primitives.unary_rate with
      | None -> state
      | Some (unrate, _) ->
        let va, state =
          compute_unary_number state modified_cc rule i in
        let () =
          store_activity
            store env counter state (2*i+1)
            rule.Primitives.syntactic_rule (fst unrate) va in
        state in
    let rec aux dep acc =
      Operator.DepSet.fold
        (fun dep (state,perts as acc) ->
           match dep with
           | Operator.ALG j ->
             let () = recompute env counter state j in
             aux (Model.get_alg_reverse_dependencies env j) acc
           | Operator.PERT p ->
             (state,List_util.merge_uniq Mods.int_compare [p] perts)
           | Operator.RULE i ->
             let rule = Model.get_rule env i in
             let pattern_va = Instances.number
                 state.instances rule.Primitives.connected_components in
             let () =
               store_activity store env counter state (2*i)
                 rule.Primitives.syntactic_rule
                 (fst rule.Primitives.rate) pattern_va in
             let state = pop_exact_matchings state i in
             (state,perts))
        dep acc in
    let state',perts = aux deps (state,[]) in
    let state'' =
      if Hashtbl.length changed_connectivity = 0 then state'
      else Model.fold_rules (unary_rule_update changed_connectivity) state' env in
    ({state'' with
      outdated_elements =
        state.precomputed.always_outdated,Hashtbl.create 32},perts)

  let overwrite_var i counter state expr =
    let rdeps,changed_connectivity = state.outdated_elements in
    let () =
      state.variables_overwrite.(i) <-
        Some (Alg_expr.CONST (value_alg counter state expr)) in
    {state with
     outdated_elements =
       (Operator.DepSet.add (Operator.ALG i) rdeps,changed_connectivity)}

  let update_tokens env counter state injected =
    let injected' = List.rev_map
        (fun  ((expr,_),i) -> (value_alg counter state expr,i)) injected in
    List.fold_left
      (fun st (va,i) ->
         let () = st.tokens.(i) <- Nbr.add st.tokens.(i) va in
         let deps' = Model.get_token_reverse_dependencies env i in
         if Operator.DepSet.is_empty deps' then st
         else
           let rdeps,changed_connectivity = st.outdated_elements in
           { st with outdated_elements =
                       Operator.DepSet.union rdeps deps',changed_connectivity }
      ) state injected'

  let transform_by_a_rule outputs env counter state event_kind ?path rule inj =
    let state' =
      update_tokens
        env counter state rule.Primitives.delta_tokens in
    update_edges outputs counter (Model.domain env) inj
      state' event_kind ?path rule (Model.signatures env)

  let apply_given_unary_rule ~outputs ~rule_id env counter state event_kind rule =
    let () = assert (not state.outdated) in
    let domain = Model.domain env in
    let inj,path = Instances.pick_a_unary_instance
        state.instances state.random_state domain state.edges ~rule_id rule in
    let rdeps,changed_c = state.outdated_elements in
    let state' =
      {state with
       outdated_elements =
         (Operator.DepSet.add (Operator.RULE rule_id) rdeps,changed_c)} in
    match inj with
    | None -> Clash
    | Some inj ->
      let nodes = Matching.elements_with_types
          domain rule.Primitives.connected_components inj in
      match path with
      | Some _ ->
        Success (transform_by_a_rule
                   outputs env counter state' event_kind ?path rule inj)
      | None ->
        let max_distance = match rule.Primitives.unary_rate with
          | None -> None
          | Some (_, dist_opt) ->
            (match dist_opt with
             | None -> None
             | Some d -> Some (max_dist_to_int counter state' d)) in
        match Edges.are_connected ?max_distance state.edges nodes.(0) nodes.(1) with
        | None -> Corrected
        | Some _ as path ->
          Success (transform_by_a_rule
                     outputs env counter state' event_kind ?path rule inj)

  let apply_given_rule ~outputs ?rule_id env counter state event_kind rule =
    let () = assert (not state.outdated) in
    let domain = Model.domain env in
    match pick_a_rule_instance
            state state.random_state domain state.edges ?rule_id rule with
    | None -> Clash
    | Some (inj,rev_roots) ->
      let () =
        if !Parameter.debugModeOn then
          let roots = Tools.array_rev_of_list rev_roots in
          Format.printf "@[On roots:@ @[%a@]@]@."
            (Pp.array Pp.space (fun _ -> Format.pp_print_int)) roots in
      match rule.Primitives.unary_rate with
      | None ->
        let out =
          transform_by_a_rule outputs env counter state event_kind rule inj in
        Success out
      | Some (_,max_distance) ->
        match max_distance with
        | None ->
          (match rev_roots with
           | root1 :: root0 :: [] ->
             if Edges.in_same_connected_component root0 root1 state.edges then
               Corrected
             else
               Success
                 (transform_by_a_rule outputs env counter state event_kind rule inj)
           | _ -> failwith "apply_given_rule unary rule without 2 patterns")
        | Some dist ->
          let dist' = Some (max_dist_to_int counter state dist) in
          let nodes = Matching.elements_with_types
              domain rule.Primitives.connected_components inj in
          match
            Edges.are_connected ?max_distance:dist' state.edges nodes.(0)
              nodes.(1) with
          | None ->
            Success (transform_by_a_rule
                       outputs env counter state event_kind rule inj)
          | Some _ -> Corrected

  let force_rule ~outputs env counter state event_kind rule =
    match apply_given_rule ~outputs env counter state event_kind rule with
    | Success out -> Some out
    | Corrected | Clash ->
      let () = assert (not state.outdated) in
      let unary_rate = match rule.Primitives.unary_rate with
        | None -> None
        | Some (loc, dist_opt) ->
          (match dist_opt with
           | None -> Some (loc,None)
           | Some d ->
             Some (loc,Some (max_dist_to_int counter state d))) in
      match Instances.all_injections
              ?unary_rate state.instances (Model.domain env) state.edges
              rule.Primitives.connected_components with
      | [] ->
        let () =
          ExceptionDefn.warning
            (fun f -> Format.fprintf f "At t=%f, %a does not apply (anymore)"
                (Counter.current_time counter)
                (Trace.print_event_kind ~env) event_kind) in
        None
      | l ->
        let (h,_) = List_util.random state.random_state l in
        Some (transform_by_a_rule
                outputs env counter state event_kind rule h)

  (* Redefines `adjust_rule_instances` *)
  let adjust_rule_instances ~rule_id env counter state rule =
    let () = assert (not state.outdated) in
    let domain = Model.domain env in
    let unary_rate = match rule.Primitives.unary_rate with
      | None -> None
      | Some (loc, dist_opt) ->
        (match dist_opt with
         | None -> Some (loc,None)
         | Some d ->
           Some (loc,Some (max_dist_to_int counter state d))) in
    let act,state =
      adjust_rule_instances
        ~rule_id ?unary_rate state domain state.edges
        rule.Primitives.connected_components in
    let () =
      store_activity (fun _ _ _ -> ()) env counter state (2*rule_id)
        rule.Primitives.syntactic_rule (fst rule.Primitives.rate) act in
    state

  (* Redefines `adjust_unary_rule_instances` *)
  let adjust_unary_rule_instances ~rule_id env counter state rule =
    let () = assert (not state.outdated) in
    let domain = Model.domain env in
    let max_distance =
      Option_util.bind
        (fun (_, dist_opt) ->
           Option_util.map (max_dist_to_int counter state) dist_opt)
        rule.Primitives.unary_rate in
    let act,state =
      adjust_unary_rule_instances
        ~rule_id ?max_distance state domain state.edges
        rule.Primitives.connected_components in
    let () =
      store_activity (fun _ _ _ -> ()) env counter state (2*rule_id+1)
        rule.Primitives.syntactic_rule (fst rule.Primitives.rate) act in
    state

  let incorporate_extra_pattern domain state pattern =
    let () = assert (not state.outdated) in
    let () = Instances.incorporate_extra_pattern
        state.instances pattern (Matching.roots_of domain state.edges pattern) in
    { state with outdated = false }

  let snapshot env counter fn state = {
    Data.snapshot_file = fn;
    Data.snapshot_event = Counter.current_event counter;
    Data.snapshot_time = Counter.current_time counter;
    Data.snapshot_agents =
      Edges.build_snapshot (Model.signatures env) state.edges;
    Data.snapshot_tokens = Array.mapi (fun i x ->
        (Format.asprintf "%a" (Model.print_token ~env) i,x)) state.tokens;
  }

  let apply_rule ~outputs ~maxConsecutiveClash env counter graph =
    let choice = pick_rule graph.random_state graph in
    let rule_id = choice/2 in
    let rule = Model.get_rule env rule_id in
    let cause = Trace.RULE rule_id in
    let () =
      if !Parameter.debugModeOn then
        Format.printf
          "@[<v>@[Applied@ %t%i:@]@ @[%a@]@]@."
          (fun f -> if choice mod 2 = 1 then Format.fprintf f "unary@ ")
          rule_id (Kappa_printer.elementary_rule ~env) rule
          (*Rule_interpreter.print_dist env graph rule_id*) in
    let apply_rule =
      if choice mod 2 = 1
      then apply_given_unary_rule ~outputs ~rule_id
      else apply_given_rule ~outputs ~rule_id in
    match apply_rule env counter graph cause rule with
    | Success (graph') ->
      let final_step = not (Counter.one_constructive_event counter) in
      (Some rule.Primitives.syntactic_rule,final_step,graph')
    | (Clash | Corrected) as out ->
      let continue =
        if out = Clash then
          Counter.one_clashing_instance_event counter
        else if choice mod 2 = 1
        then Counter.one_no_more_unary_event counter
        else Counter.one_no_more_binary_event counter in
      if Counter.consecutive_null_event counter < maxConsecutiveClash
      then (None,not continue,graph)
      else
        (None,not continue,
         (if choice mod 2 = 1
          then adjust_unary_rule_instances ~rule_id env counter graph rule
          else adjust_rule_instances ~rule_id env counter graph rule))

  let aux_add_tracked patterns name tests state tpattern =
    let () = state.outdated <- true in
    let () =
      Array.iter
        (fun pattern ->
           let acc = Pattern.ObsMap.get tpattern pattern in
           Pattern.ObsMap.set tpattern
             pattern ((name,patterns,tests)::acc))
        patterns in
    { state with outdated = false }

  let add_tracked patterns name tests state =
    let () = assert (not state.outdated) in
    match state.story_machinery with
    | None ->
      let () =
        ExceptionDefn.warning
          (fun f -> Format.fprintf f
              "Observable %s should be tracked but the trace is not stored"
              name) in
      state
    | Some tpattern -> aux_add_tracked patterns name tests state tpattern
  let remove_tracked patterns name state =
    let () = assert (not state.outdated) in
    match state.story_machinery with
    | None -> state
    | Some tpattern ->
      match name with
      | None ->
        let () = state.outdated <- true in
        let tester (_,el,_) =
          not @@
          Tools.array_fold_lefti
            (fun i b x -> b && Pattern.is_equal_canonicals x el.(i))
            true patterns in
        let () =
          Array.iter
            (fun pattern ->
               let acc = Pattern.ObsMap.get tpattern pattern in
               Pattern.ObsMap.set tpattern pattern (List.filter tester acc))
            patterns in
        { state with outdated = false }
      | Some name ->
        let () = state.outdated <- true in
        let tester (n,_,_) = not((String.compare name n) = 0) in
        let tpattern' =
          Pattern.ObsMap.map
            (fun plist -> List.filter tester plist) tpattern in
        { state with outdated = false; story_machinery = Some tpattern' }

  let add_tracked_species patterns name tests state =
    aux_add_tracked patterns name tests state state.species

  let remove_tracked_species name state =
    let () = state.outdated <- true in
    let tester (n,_,_) = not((String.compare name n) = 0) in
    let species' =
      Pattern.ObsMap.map
        (fun plist -> List.filter tester plist) state.species in
    { state with outdated = false; species = species' }

  let get_random_state state = state.random_state


  let send_instances_message msg state = 
    { state with instances = Instances.send_message msg state.instances}

end