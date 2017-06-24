(**
  * export.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: December, the 9th of 2014
  * Last modification: Time-stamp: <May 16 2017>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let warn parameters mh pos exn default =
  Exception.warn parameters mh pos exn default

(****************************************************************)
(*module signatures*)

module Export =
  functor (Reachability:Analyzer.Analyzer) ->
  struct

type state =
  (Reachability.static_information,
   Reachability.dynamic_information)
    Remanent_state.state

type contact_map = Public_data.contact_map

type ctmc_flow = Remanent_state.flow

type ode_flow = Ode_fragmentation_type.ode_frag

type c_compilation = Cckappa_sig.compil

type reachability_analysis =
  (Reachability.static_information,
   Reachability.dynamic_information)
    Remanent_state.reachability_result

type parameters = Remanent_parameters_sig.parameters

type errors = Exception.method_handler

type internal_contact_map = Remanent_state.internal_contact_map

type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t *
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type handler = Cckappa_sig.kappa_handler

type internal_constraints_list = Remanent_state.internal_constraints_list

(******************************************************************)

let string_of_influence_node x =
  match x with
  | Remanent_state.Rule i -> "Rule "^(string_of_int i)
  | Remanent_state.Var i -> "Var "^(string_of_int i)

let print_influence_map parameters influence_map =
  let log = (Remanent_parameters.get_logger parameters) in
  Loggers.fprintf log "Influence map:" ;
  Loggers.print_newline log;
  Remanent_state.InfluenceNodeMap.iter
    (fun x y ->
       Remanent_state.InfluenceNodeMap.iter
         (fun y _labellist ->
            let () =
              Loggers.fprintf log
                " %s->%s"
                (string_of_influence_node x)
                (string_of_influence_node y)
            in
            let () =
              Loggers.print_newline log in
            ())
         y)
    influence_map.Remanent_state.positive;
  Remanent_state.InfluenceNodeMap.iter
    (fun x y ->
       Remanent_state.InfluenceNodeMap.iter
         (fun y _labellist ->
            let () =
              Loggers.fprintf log
                " %s-|%s"
                (string_of_influence_node x) (string_of_influence_node y) in
            let () = Loggers.print_newline log in
            ())
         y)
    influence_map.Remanent_state.negative;
  Loggers.print_newline log

let query_inhibition_map influence_map r1 r2 =
  match
    Remanent_state.InfluenceNodeMap.find_option
      (Remanent_state.Rule r1) influence_map.Remanent_state.negative
  with
  | None -> []
  | Some map ->
    begin
      match
        Remanent_state.InfluenceNodeMap.find_option
              (Remanent_state.Rule r2)
              map
      with
      | None -> []
      | Some l -> l
    end

let print_contact_map parameters contact_map =
  let log = (Remanent_parameters.get_logger parameters) in
  Loggers.fprintf log  "Contact map: ";
  Loggers.print_newline log;
  Mods.StringSetMap.Map.iter
    (fun x ->
       Mods.StringSetMap.Map.iter
         (fun y (l1,l2) ->
            if l1<>[]
            then
              begin
                let () = Loggers.fprintf log "%s@%s: " x y in
                let _ = List.fold_left
                    (fun bool x ->
                       (if bool then
                          Loggers.fprintf log ", ");
                       Loggers.fprintf log "%s" x;
                       true)
                    false l1
                in
                Loggers.print_newline log
              end
            else ();
            List.iter
              (fun (z,t) ->
                 Loggers.fprintf log
                   "%s@%s--%s@%s" x y z t;
                 Loggers.print_newline log
              ) l2
         )
    ) contact_map

(*---------------------------------------------------------------------------*)
(*operations of module signatures*)

let init ?compil ~called_from () =
  match compil with
  | Some compil ->
    let parameters = Remanent_parameters.get_parameters ~called_from () in
    let state =
      Remanent_state.create_state parameters
        (Remanent_state.Compil compil)
    in
    state
  | None ->
    begin
      match
        called_from
      with
      | Remanent_parameters_sig.Internalised
      | Remanent_parameters_sig.Server
      | Remanent_parameters_sig.KaSim
      | Remanent_parameters_sig.KaSa ->
        begin
          let errors = Exception.empty_error_handler in
          let errors, parameters, files  = Get_option.get_option errors in
          let log = (Remanent_parameters.get_logger parameters) in
          let _ =
            Loggers.fprintf log "%s"
              (Remanent_parameters.get_full_version parameters)
          in
          let () = Loggers.print_newline log in
          let _ =
            Loggers.fprintf log "%s"
              (Remanent_parameters.get_launched_when_and_where parameters)
          in
          let () = Loggers.print_newline log in
          Remanent_state.create_state ~errors parameters
            (Remanent_state.Files files)
        end
    end

let get_parameters = Remanent_state.get_parameters
let set_parameters = Remanent_state.set_parameters
let set_errors = Remanent_state.set_errors
let get_errors = Remanent_state.get_errors

let title_only_in_kasa parameters =
  match
    Remanent_parameters.get_called_from parameters
  with
  | Remanent_parameters_sig.Server
  | Remanent_parameters_sig.Internalised
  | Remanent_parameters_sig.KaSim -> false
  | Remanent_parameters_sig.KaSa -> true

let compute_show_title do_we_show_title log_title =
  (fun state ->
     let parameters = Remanent_state.get_parameters state in
     if do_we_show_title parameters
     then
       match log_title with
       | None -> ()
       | Some title ->
         let title =
           if title_only_in_kasa parameters
           then title ^ "..."
           else
             "+ " ^ title
         in
         let () =
           Loggers.fprintf
             (Remanent_parameters.get_logger parameters) "%s" title
         in
         Loggers.print_newline (Remanent_parameters.get_logger parameters)
     else
       ())

let get_gen
    ?debug_mode
    ?dump_result
    ?stack_title
    ?do_we_show_title
    ?log_title
    ?log_main_title
    ?log_prefix
    ?phase
    ?int
    ?dump
    get compute state =
  let debug_mode =
    match debug_mode with
    | None | Some false -> false
    | Some true -> true
  in
  let dump_result =
    match dump_result with
    | None | Some false -> false
    | Some true -> true
  in
  let dump =
    match dump with
    | None -> (fun state _output -> state)
    | Some f -> f
  in
  let do_we_show_title =
    match do_we_show_title with
    | None -> (fun _ -> true)
    | Some f -> f
  in
  (*------------------------------------------------------*)
  match get state with
  | None ->
    let parameters = Remanent_state.get_parameters state in
    let parameters' =
      Remanent_parameters.update_call_stack
        parameters debug_mode stack_title
    in
    let parameters' =
      match log_prefix with
      | None -> parameters'
      | Some prefix -> Remanent_parameters.set_prefix parameters' prefix
    in
    let state = Remanent_state.set_parameters parameters' state in
    let () =
      match log_main_title with
      | None -> ()
      | Some title ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters) "%s" title
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameters')
    in
    let show_title = compute_show_title do_we_show_title log_title in
    (*------------------------------------------------------*)
    let state =
      match phase with
      | None -> state
      | Some phase -> Remanent_state.add_event phase int state
    in
    let state, output = compute show_title state in
    (*------------------------------------------------------*)
    let state =
      match phase with
      | None -> state
      | Some phase -> Remanent_state.close_event phase int state
    in
    (*------------------------------------------------------*)
    let state =
      if
        Remanent_parameters.get_trace parameters' || dump_result
      then
        dump state output
      else
        state
    in
    Remanent_state.set_parameters parameters state, output
  | Some a ->
  state, a

let lift_wo_handler f = (fun parameter error _handler x -> f parameter error x)

let flush_errors state =
  Remanent_state.set_errors Exception.empty_error_handler state

let compute_env_init
    show_title
    (state:
       (Reachability.static_information,
        Reachability.dynamic_information)
         Remanent_state.state)
  =
  match Remanent_state.get_init state with
  | Remanent_state.Compil compil -> state, None, None, None
  | Remanent_state.Files files ->
    let () = show_title state in
    let cli = Run_cli_args.default in
    let () = cli.Run_cli_args.inputKappaFileNames <- files in
    let (_,_,env, contactmap, _, _, _, _, init), _ =
      Cli_init.get_compilation cli
    in
    let state =
      Remanent_state.set_init_state
        (Some init)
        (Remanent_state.set_env (Some env)
           (Remanent_state.set_contact_map_int
              (Some contactmap) state))
    in
    state, Some (env:Model.t), Some (init), Some (contactmap)

let compute_env show_title state =
  let state, env, _, _ = compute_env_init show_title state in
  state, env

let get_env =
  get_gen
    ~phase:StoryProfiling.LKappa_signature
    Remanent_state.get_env
    compute_env

let compute_init show_title state =
  let state, _, init, _ = compute_env_init show_title state in
  state, init

let get_init =
  get_gen
    ~phase:StoryProfiling.LKappa_signature
    Remanent_state.get_init_state
    compute_init

let compute_contact_map_int show_title state =
  let state, _, _, contactmap =
    compute_env_init show_title state in
  state, contactmap

let get_contact_map_int =
    get_gen
      ~phase:StoryProfiling.LKappa_signature
      Remanent_state.get_contact_map_int
      compute_contact_map_int

(******************************************************************)
(*compilation*)

let compute_compilation show_title state =
  let compil =
    match Remanent_state.get_init state with
    | Remanent_state.Compil compil -> compil
    | Remanent_state.Files files ->
      let () = show_title state in
      Cli_init.get_ast_from_list_of_files files
  in
  let state = Remanent_state.set_compilation compil state in
  state, compil

let get_compilation =
  get_gen
    ~phase:StoryProfiling.KaSa_lexing
    Remanent_state.get_compilation
    compute_compilation

(******************************************************************)

let compute_refined_compil show_title state =
  let state,compil = get_compilation state in
  let errors = Remanent_state.get_errors state in
  let parameters = Remanent_state.get_parameters state in
  let () = show_title state in
  let errors,refined_compil =
    Prepreprocess.translate_compil parameters errors compil
  in
  let state = Remanent_state.set_errors errors state in
  let state = Remanent_state.set_refined_compil refined_compil state in
  state, refined_compil

let get_refined_compil =
  get_gen
    ~debug_mode:Preprocess.local_trace
    ~stack_title:"Prepreprocess.translate_compil"
    ~phase:StoryProfiling.KaSim_compilation
    Remanent_state.get_refined_compil
    compute_refined_compil

let compute_prehandler show_title state =
  let state, refined_compil = get_refined_compil state in
  let parameters = Remanent_state.get_parameters state in
  let errors = Remanent_state.get_errors state in
  let () = show_title state in
  let errors, handler =
    List_tokens.scan_compil parameters errors refined_compil
  in
  let state = Remanent_state.set_errors errors state in
  let state = Remanent_state.set_handler handler state in
  state, handler

let lift_dump_parameter_error dump state output =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = dump parameters error output in
  Remanent_state.set_errors error state

let get_prehandler =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~dump_result:Print_handler.trace
    ~stack_title:"List_tokens.scan_compil"
    ~log_prefix:"Signature:"
    ~phase:StoryProfiling.KaSa_lexing
    Remanent_state.get_handler
    compute_prehandler
    ~dump:(lift_dump_parameter_error Print_handler.print_handler)

let compute_c_compilation_handler show_title state =
  let parameters = Remanent_state.get_parameters state in
  let state, refined_compil = get_refined_compil state in
  let state, handler = get_prehandler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error, handler, c_compil =
    Preprocess.translate_c_compil
      parameters error handler refined_compil
  in
  Remanent_state.set_errors
    error
    (Remanent_state.set_handler handler
       (Remanent_state.set_c_compil c_compil state)),
  (c_compil,handler)

(******************************************************************)

let choose f show_title state =
  let state,pair = compute_c_compilation_handler show_title state in
  state,f pair

let get_c_compilation =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~stack_title:"Preprocess.translate_c_compil"
    ~log_prefix:"Compilation:"
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compiling"
    ~phase:StoryProfiling.KaSa_linking
    Remanent_state.get_c_compil (choose fst)

let get_handler =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~stack_title:"Preprocess.translate_c_compil"
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compiling"
    ~phase:StoryProfiling.KaSa_linking
    Remanent_state.get_handler (choose snd)

let dump_c_compil state c_compil =
  let state, handler = get_handler state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = Print_cckappa.print_compil parameters error handler c_compil in
  let state = Remanent_state.set_errors error state in
  state

let compute_raw_internal_contact_map show_title state =
  let state, _ = get_compilation  state in
  let state, handler = get_handler state in
  let () = show_title state in
  let state, c_compil = get_c_compilation state in
  let parameters = Remanent_state.get_parameters state in
  let parameters = Remanent_parameters.update_prefix  parameters "Compilation:" in
  let error = Remanent_state.get_errors state in
  let error =
    if Remanent_parameters.get_trace parameters || Print_cckappa.trace
    then Print_cckappa.print_compil parameters error handler c_compil
    else error
  in
  let error, contact_map =
    Preprocess.export_contact_map parameters error handler
  in
  let state = Remanent_state.set_errors error state in
  Remanent_state.set_internal_contact_map Public_data.Low contact_map state,
  contact_map

let dump_raw_internal_contact_map state handler =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = Print_handler.dot_of_contact_map parameters error handler in
  Remanent_state.set_errors error state

let get_raw_internal_contact_map  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Generating the raw contact map"
    (*  ~dump:dump_raw_internal_contact_map *)
    (Remanent_state.get_internal_contact_map Public_data.Low)
    compute_raw_internal_contact_map

let compute_raw_contact_map show_title state =
  let sol        = ref Mods.StringSetMap.Map.empty in
  let state, handler = get_prehandler state in
  let parameters = Remanent_state.get_parameters state in
  let error      = Remanent_state.get_errors state in
  let add_link (a,b) (c,d) sol =
    let sol_a = Mods.StringSetMap.Map.find_default
        Mods.StringSetMap.Map.empty a sol in
    let l,old = Mods.StringSetMap.Map.find_default
        ([],[]) b sol_a in
    Mods.StringSetMap.Map.add a
      (Mods.StringSetMap.Map.add b (l,((c,d)::old)) sol_a) sol
  in
  (*----------------------------------------------------------------*)
  let add_internal_state (a,b) c sol =
    match c with
    | Ckappa_sig.Binding _ -> sol
    | Ckappa_sig.Internal state ->
      let sol_a = Mods.StringSetMap.Map.find_default
          Mods.StringSetMap.Map.empty a sol in
      let old,l = Mods.StringSetMap.Map.find_default ([],[]) b sol_a in
      Mods.StringSetMap.Map.add a
        (Mods.StringSetMap.Map.add b (state::old,l) sol_a) sol
  in
  (*----------------------------------------------------------------*)
  let simplify_site site =
    match site with
    | Ckappa_sig.Binding site_name
    | Ckappa_sig.Internal site_name -> site_name
  in
  (*----------------------------------------------------------------*)
  let () = show_title state in
  let error =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.iter
      parameters error
      (fun parameters error (i,j) s  ->
         let error,ag =
           Handler.translate_agent
             ~message:"unknown agent type" ~ml_pos:(Some __POS__)
             parameters error handler i
         in
         let error,site =
           Handler.translate_site parameters error handler i j
         in
         let site = simplify_site site in
         let error =
           Ckappa_sig.Dictionary_of_States.iter
             parameters error
             (fun _parameters error _s state  () () ->
                let () =
                  sol := add_internal_state (ag,site) state (!sol)
                in
                error)
             s
         in
         error)
      handler.Cckappa_sig.states_dic
  in
  (*----------------------------------------------------------------*)
  let sol = !sol in
  let error, sol =
    Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameters error
      (fun _parameters error (i, (j , _k)) (i', j', _k') sol ->
         let error, ag_i =
           Handler.translate_agent
             ~message:"unknown agent type" ~ml_pos:(Some __POS__)
             parameters error handler i
         in
         let error, site_j =
           Handler.translate_site parameters error handler i j
         in
         let site_j = simplify_site site_j in
         let error, ag_i' =
           Handler.translate_agent
             ~message:"unknown agent type" ~ml_pos:(Some __POS__)
             parameters error handler i'
         in
         let error, site_j' =
           Handler.translate_site parameters error handler i' j'
         in
         let site_j' = simplify_site site_j' in
         let sol = add_link (ag_i,site_j) (ag_i',site_j') sol in
         error, sol)
      handler.Cckappa_sig.dual sol
  in
  let sol =
    Mods.StringSetMap.Map.map
      (Mods.StringSetMap.Map.map (fun (l,x) -> List.rev l,x)) sol
  in
  Remanent_state.set_errors error
    (Remanent_state.set_contact_map Public_data.Low sol state),
  sol

let get_raw_contact_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compute the contact map"
    (Remanent_state.get_contact_map Public_data.Low)
    compute_raw_contact_map

let convert_label a =
  if a<0 then Remanent_state.Side_effect (-(a+1))
  else Remanent_state.Direct a

let convert_id x nrules =
  if x<nrules
  then
    Remanent_state.Rule x
  else
    Remanent_state.Var (x-nrules)

let convert_half_influence_map influence nrules  =
  Ckappa_sig.PairRule_setmap.Map.fold
    (fun (x,y) list map ->
       let x = convert_id (int_of_string (Ckappa_sig.string_of_rule_id x))
           nrules in
       let y = convert_id (int_of_string (Ckappa_sig.string_of_rule_id y))
           nrules in
       let old =
         match
           Remanent_state.InfluenceNodeMap.find_option x map
         with
         | None -> Remanent_state.InfluenceNodeMap.empty
         | Some x -> x
       in
       let list =
         Quark_type.Labels.convert_label_set_couple list
       in
       let list =
         List.rev_map
           (fun (a,b) -> convert_label a,convert_label b)
           (List.rev list)
       in
       Remanent_state.InfluenceNodeMap.add x
         (Remanent_state.InfluenceNodeMap.add y list old)
         map
    )
    influence
    Remanent_state.InfluenceNodeMap.empty

let compute_quark_map show_title state =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let error,quark_map =
    Quark.quarkify parameters error handler c_compil
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters)
      || Print_quarks.trace
    then
      Print_quarks.print_quarks parameters error handler quark_map
    else
      error
  in
  Remanent_state.set_errors error
    (Remanent_state.set_quark_map quark_map state),
  quark_map

let get_quark_map =
  get_gen
    ~debug_mode:Quark.local_trace
    ~stack_title:"Quark.quarkify"
    ~log_prefix:"Quarks:"
    Remanent_state.get_quark_map
    compute_quark_map

let compute_raw_internal_influence_map show_title state =
  let parameters = Remanent_state.get_parameters state in
  let state, compil = get_c_compilation state in
  let state, quark_map = get_quark_map state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let nrules = Handler.nrules parameters error handler in
  let () = show_title state in
  let error,wake_up_map,inhibition_map =
    Influence_map.compute_influence_map parameters
      error handler quark_map nrules
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters  || Print_quarks.trace)
      && Remanent_parameters.get_influence_map_accuracy_level parameters = Remanent_parameters_sig.Low
    then
      Print_quarks.print_wake_up_map
        parameters
        error
        handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels "\n"
        wake_up_map
    else error
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters || Print_quarks.trace)
      && Remanent_parameters.get_influence_map_accuracy_level parameters = Remanent_parameters_sig.Low
    then
      Print_quarks.print_inhibition_map
        parameters error handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels
        "\n"
        inhibition_map
    else error
  in
  let state =
    Remanent_state.set_internal_influence_map Public_data.Low
      (wake_up_map,inhibition_map)
      state
  in
  Remanent_state.set_errors error state,
  (wake_up_map, inhibition_map)

let get_raw_internal_influence_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_prefix:"Influence_map:"
    ~log_main_title:"Generating the raw influence map..."
    ~phase:(StoryProfiling.Internal_influence_map "raw")
    (Remanent_state.get_internal_influence_map Public_data.Low)
    compute_raw_internal_influence_map

module AgentProj =
  Map_wrapper.Proj
    (Ckappa_sig.Agent_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))

module SiteProj =
  Map_wrapper.Proj
    (Ckappa_sig.Site_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))

module StateProj =
  Map_wrapper.Proj
    (Ckappa_sig.State_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))

(******************************************************************)

let convert_contact_map show_title state contact_map =
  let parameters = Remanent_state.get_parameters state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error, contact_map =
    AgentProj.monadic_proj_map_i
      (fun parameters error ag ->
         Handler.translate_agent
           ~message:"unknown agent type" ~ml_pos:(Some __POS__)
           parameters error handler ag)
      parameters error
      Mods.StringSetMap.Map.empty
      (fun parameters error _ ag sitemap->
         SiteProj.monadic_proj_map_i
           (fun parameters errors site ->
              let error, site = Handler.translate_site parameters errors
                  handler ag (site:Ckappa_sig.c_site_name) in
              error, Handler.print_site_contact_map site)
           parameters error
           ([],[])
           (fun parameters error (list_a,list_b) site (list_a',list_b') ->
              let error, list_a'' =
                List.fold_left
                  (fun (error, list) state ->
                     let error, state = Handler.translate_state parameters
                         error handler ag site state in
                     match state with
                     | Ckappa_sig.Internal state -> error, state::list
                     | Ckappa_sig.Binding _ ->
                       warn parameters error __POS__ Exit list)
                  (error, list_a) (List.rev list_a')
              in
              let error, list_b'' =
                List.fold_left
                  (fun (error, list) (agent,site) ->
                     let error, ag =
                       Handler.translate_agent
                         ~message:"unknown agent type" ~ml_pos:(Some __POS__)
                         parameters error handler agent
                     in
                     let error, site = Handler.translate_site parameters error
                         handler agent site in
                     let st = Handler.print_site_contact_map site in
                     error, (ag,st)::list)
                  (error, list_b) (List.rev list_b')
              in
              error, (list_a'', list_b''))
           sitemap)
      contact_map
  in
  Remanent_state.set_errors error state,
  contact_map

(******************************************************************)

let convert_influence_map show_title state (wake_up_map, inhibition_map) =
  let parameters = Remanent_state.get_parameters state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let nrules = Handler.nrules parameters error handler in
  let state = Remanent_state.set_errors error state in
  let output =
    {
      Remanent_state.positive = convert_half_influence_map wake_up_map nrules ;
      Remanent_state.negative =
        convert_half_influence_map inhibition_map nrules ;
    }
  in
  state,
  output

let compute_intermediary_internal_influence_map show_title state =
  let state, handler = get_handler state in
  let state, compil = get_c_compilation state in
  let state,(wake_up_map,inhibition_map) =
    get_raw_internal_influence_map state
  in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error,wake_up_map =
    Algebraic_construction.filter_influence
      parameters error handler compil wake_up_map true
  in
  let error,inhibition_map =
    Algebraic_construction.filter_influence
      parameters error handler compil inhibition_map false
  in
  let state =
    Remanent_state.set_internal_influence_map Public_data.Medium
      (wake_up_map,inhibition_map)
      state
  in
  let state = Remanent_state.set_errors error state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let nrules = Handler.nrules parameters error handler in
  let state =
    Remanent_state.set_influence_map Public_data.Medium
      {
        Remanent_state.positive =
          convert_half_influence_map wake_up_map nrules;
        Remanent_state.negative =
          convert_half_influence_map inhibition_map nrules ;
      }
      state
  in
  let error =
    if Remanent_parameters.get_trace parameters || Print_quarks.trace
    then
      Print_quarks.print_wake_up_map
        parameters
        error
        handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels "\n"
        wake_up_map
    else error
  in
  let error =
    if
      Remanent_parameters.get_trace parameters
      || Print_quarks.trace
    then
      Print_quarks.print_inhibition_map
        parameters error handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels
        "\n"
        inhibition_map
    else error
  in
  Remanent_state.set_errors error state, (wake_up_map, inhibition_map)

let compute_map_gen
    (get: ?accuracy_level:Public_data.accuracy_level ->
     (Reachability.static_information,
      Reachability.dynamic_information)
       Remanent_state.state ->
     (Reachability.static_information,
      Reachability.dynamic_information)
       Remanent_state.state * 'a )
    store convert ?(accuracy_level=Public_data.Low)
    ?do_we_show_title:(do_we_show_title=(fun _ -> true))
    ?log_title
    state =
  let show_title =
    match log_title with
    | None -> (fun _ -> ())
    | Some log_title ->
      compute_show_title do_we_show_title (log_title accuracy_level)
  in
  let () = show_title state in
  let state, internal =
    get ~accuracy_level state
  in
  let state, rep = convert (fun _ -> ()) state internal in
  store accuracy_level rep state, rep


let get_intermediary_internal_influence_map =
  get_gen
    ~log_prefix:"Influence_map:"
    ~log_title:"Refining the influence map"
    ~phase:(StoryProfiling.Internal_influence_map "medium")
    (Remanent_state.get_internal_influence_map Public_data.Medium)
    compute_intermediary_internal_influence_map

let compute_reachability_result show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let bdu_handler = Remanent_state.get_bdu_handler state in
  let log_info = Remanent_state.get_log_info state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, log_info, static, dynamic =
    Reachability.main
      parameters log_info error bdu_handler c_compil handler
  in
  let error, dynamic, state =
    Reachability.export static dynamic
      error state
  in
  let state = Remanent_state.set_errors error state in
  let state = Remanent_state.set_log_info log_info state in
  let state = Remanent_state.set_bdu_handler bdu_handler state in
  let state = Remanent_state.set_reachability_result (static, dynamic) state in
  state, (static, dynamic)

let get_reachability_analysis =
  get_gen
    ~log_title:"Reachability analysis"
    (Remanent_state.get_reachability_result)
    compute_reachability_result

let compute_high_res_internal_influence_map show_title state =
  let state, handler = get_handler state in
  let state, compil = get_c_compilation state in
  let state,(wake_up_map,inhibition_map) =
    get_intermediary_internal_influence_map state
  in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let state, (static, dynamic) = get_reachability_analysis state in
  let () = show_title state in
  let maybe_reachable static dynamic error =
    Reachability.maybe_reachable static dynamic error Analyzer_headers.Morphisms
  in
  let (error,dynamic), wake_up_map =
    Algebraic_construction.filter_influence_high
      maybe_reachable
      parameters handler error compil
      static dynamic wake_up_map true
  in
  let (error,dynamic), inhibition_map =
    Algebraic_construction.filter_influence_high
      maybe_reachable
      parameters handler error compil
      static dynamic inhibition_map false
  in
  let state = Remanent_state.set_reachability_result (static, dynamic) state in
  let state =
    Remanent_state.set_internal_influence_map Public_data.High
      (wake_up_map,inhibition_map)
      state
  in
  let state = Remanent_state.set_errors error state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let nrules = Handler.nrules parameters error handler in
  let state =
    Remanent_state.set_influence_map Public_data.High
      {
        Remanent_state.positive =
          convert_half_influence_map wake_up_map nrules;
        Remanent_state.negative =
          convert_half_influence_map inhibition_map nrules ;
      }
      state
  in
  let error =
    if Remanent_parameters.get_trace parameters || Print_quarks.trace
    then
      Print_quarks.print_wake_up_map
        parameters
        error
        handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels "\n"
        wake_up_map
    else error
  in
  let error =
    if
      Remanent_parameters.get_trace parameters
      || Print_quarks.trace
    then
      Print_quarks.print_inhibition_map
        parameters error handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels
        "\n"
        inhibition_map
    else error
  in
  Remanent_state.set_errors error state, (wake_up_map, inhibition_map)

  let get_high_res_internal_influence_map =
    get_gen
      ~log_prefix:"Influence_map:"
      ~log_title:"Refining further the influence map"
      ~phase:(StoryProfiling.Internal_influence_map "high")
      (Remanent_state.get_internal_influence_map Public_data.High)
      compute_high_res_internal_influence_map


let get_internal_influence_map
    ?accuracy_level:(accuracy_level=Public_data.Low)
    state =
  match accuracy_level with
  | Public_data.Low ->
    get_raw_internal_influence_map state
  | Public_data.Medium -> get_intermediary_internal_influence_map state
  | Public_data.High | Public_data.Full ->
    get_high_res_internal_influence_map state

let compute_influence_map
    ?accuracy_level:(accuracy_level=Public_data.Low) _show_title =
  compute_map_gen
    get_internal_influence_map
    Remanent_state.set_influence_map
    convert_influence_map
    ~accuracy_level

let get_influence_map
    ?accuracy_level:(accuracy_level=Public_data.Low)
    ?do_we_show_title:(do_we_show_title=(fun _ -> true))
    ?log_title:(log_title=
                (fun x ->
                    match x with
                    | Public_data.Low ->
                      Some "Compute the influence map"
                    | Public_data.Medium
                    | Public_data.High | Public_data.Full ->
                      Some "Refine the influence map"))
     =
     get_gen
       (Remanent_state.get_influence_map accuracy_level)
       (compute_influence_map ~accuracy_level ~do_we_show_title ~log_title)


let query_inhibition_map ?accuracy_level state r1 r2 =
  let state,inf_map = get_influence_map ?accuracy_level state in
  let output = query_inhibition_map inf_map r1 r2 in
  state, output
(******************************************************************)

let compute_dead_agents _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_dead_agents state
  with
  | Some map -> state, map
  | None -> assert false

let get_dead_agents  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Detecting which agents may occur during simulations"
    (*  ~dump:dump_raw_internal_contact_map *)
    Remanent_state.get_dead_agents
    compute_dead_agents

(******************************************************************)

let compute_intermediary_internal_contact_map _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_internal_contact_map Public_data.Medium state
  with
  | Some map -> state, map
  | None -> assert false

let get_intermediary_internal_contact_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Generating the intermediary contact map"
    (*  ~dump:dump_raw_internal_contact_map *)
    (Remanent_state.get_internal_contact_map Public_data.Medium)
    compute_intermediary_internal_contact_map

let get_internal_contact_map
    ?accuracy_level:(accuracy_level=Public_data.Low) state =
  match
    accuracy_level
  with
  | Public_data.Low -> get_raw_internal_contact_map state
  | Public_data.Medium
  | Public_data.High
  | Public_data.Full -> get_intermediary_internal_contact_map state

let compute_contact_map
    ?accuracy_level:(accuracy_level=Public_data.Low) _show_title =
  compute_map_gen
    get_internal_contact_map
    Remanent_state.set_contact_map
    convert_contact_map
    ~accuracy_level

let get_contact_map
    ?accuracy_level:(accuracy_level=Public_data.Low)
  =
  get_gen
    (Remanent_state.get_contact_map accuracy_level)
    (compute_contact_map
       ~accuracy_level
       ~do_we_show_title:(fun _ -> true)
       ~log_title:(fun x ->
         match x with
         | Public_data.Low ->
           Some "Compute the contact map"
         | Public_data.Medium
         | Public_data.High | Public_data.Full ->
           Some "Refine the contact map"))

let get_internal_contact_map
      ?accuracy_level:(accuracy_level=Public_data.Low)
      state =
  match accuracy_level with
  | Public_data.Low ->
    get_raw_internal_contact_map state
  | Public_data.Medium | Public_data.High | Public_data.Full ->
    get_intermediary_internal_contact_map state

(******************************************************************)

let compute_signature show_title state =
  let state,l = get_contact_map state in
  let () = show_title state in
  let l =
    Mods.StringSetMap.Map.fold
      (fun a interface list ->
         (Locality.dummy_annot a,
          NamedDecls.create
            (Array.of_list
               (Mods.StringSetMap.Map.fold
                  (fun x (states,binding) acc ->
                     let binding' =
                       List.map
                         (fun (x,y) ->
                            (Locality.dummy_annot x, Locality.dummy_annot y))
                         binding in
                     (Locality.dummy_annot x,
                      (NamedDecls.create
                         (Tools.array_map_of_list
                            (fun i -> (Locality.dummy_annot i,())) states),
                       binding'))::acc)
                  interface [])))::list)
      l [] in
  let signature = Signature.create true (Array.of_list l) in
  Remanent_state.set_signature signature state,
  signature

let get_signature =
  get_gen
    Remanent_state.get_signature
    compute_signature

(******************************************************************)

let compute_ctmc_flow show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, output =
    Stochastic_classes.stochastic_classes parameters error handler c_compil in
  Remanent_state.set_errors
    error
    (Remanent_state.set_ctmc_flow output state),
  output

let get_ctmc_flow =
  get_gen
    ~log_prefix:"Flow of information in the CTMC semantics:"
    ~log_title:"Flow of information in the CTMC semantcs:"
    Remanent_state.get_ctmc_flow
    compute_ctmc_flow

let compute_ode_flow show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, output =
    Ode_fragmentation.scan_rule_set
      parameters error handler c_compil
  in
  Remanent_state.set_errors
    error
    (Remanent_state.set_ode_flow output state),
  output

(******************************************************************)

let get_ode_flow =
  get_gen
    ~log_prefix:"Flow of information in the ODE semantics:"
    ~log_title:"Flow of information in the ODE semantcs:"
    Remanent_state.get_ode_flow
    compute_ode_flow

let find_most_precise map =
  match
    Public_data.AccuracyMap.max_key map
  with
  | None -> None
  | Some key ->
    Public_data.AccuracyMap.find_option key map

let get_most_accurate_contact_map state =
  let map = Remanent_state.get_contact_map_map state in
  find_most_precise map

let get_most_accurate_influence_map state =
  let map = Remanent_state.get_influence_map_map state in
  find_most_precise map

let dump_influence_map ?accuracy_level:(accuracy_level=Public_data.Low) state =
  match
    Remanent_state.get_influence_map accuracy_level state
  with
  | None -> ()
  | Some influence_map ->
    print_influence_map (Remanent_state.get_parameters state) influence_map

let output_internal_influence_map ?logger
    ?accuracy_level:(accuracy_level=Public_data.Low) state =
  let parameters = get_parameters state in
  let state, influence_map = get_internal_influence_map ~accuracy_level state in
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let error = get_errors state in
  let error =
    Print_quarks.dot_of_influence_map ?logger parameters error handler c_compil
      influence_map
  in
  set_errors error state

let output_best_internal_influence_map state =
  let map = Remanent_state.get_internal_influence_map_map state in
  match
    Public_data.AccuracyMap.max_key map
  with
  | None -> state
  | Some accuracy_level ->
    output_internal_influence_map ~accuracy_level state

let dump_contact_map accuracy state =
  match
    Remanent_state.get_contact_map accuracy state
  with
  | None -> ()
  | Some contact_map ->
    print_contact_map (Remanent_state.get_parameters state) contact_map

let output_internal_contact_map ?logger
    ?accuracy_level:(accuracy_level=Public_data.Low) state =
  let parameters = Remanent_state.get_parameters state in
  let state, contact_map = get_internal_contact_map ~accuracy_level state in
  let state, handler = get_handler state in
  let error = get_errors state in
  let error =
    Preprocess.dot_of_contact_map ?logger parameters error handler contact_map
  in
  set_errors error state

(******************************************************************)

let dump_signature state =
  match Remanent_state.get_signature state with
  | None -> ()
  | Some _signature -> ()

let dump_errors state =
  Exception.print (Remanent_state.get_parameters state)
    (Remanent_state.get_errors state)

let dump_errors_light state =
  Exception.print_errors_light_for_kasim
    (Remanent_state.get_parameters state)
    (Remanent_state.get_errors state)

(******************************************************************)

let compute_dead_rules _show_title state =
  let state, _ = get_reachability_analysis state in
  match
    Remanent_state.get_dead_rules state
  with
  | Some l -> state, l
  | None -> assert false

let get_dead_rules  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Detecting which rules may be triggered during simulations"
    (*  ~dump:dump_raw_internal_contact_map *)
    Remanent_state.get_dead_rules
    compute_dead_rules

(****************************************************************)

let compute_internal_constraints_list _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_internal_constraints_list state
  with
  | None ->
    let error = Remanent_state.get_errors state in
    let parameters = Remanent_state.get_parameters state in
    let error, output = warn parameters error __POS__ Exit [] in
    let state = Remanent_state.set_errors error state in
    state, output
  | Some output -> state, output

let get_internal_constraints_list =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Extract refinement lemmas"
    Remanent_state.get_internal_constraints_list
    compute_internal_constraints_list

let compute_constraints_list _show_title state =
  let error = Remanent_state.get_errors state in
  let state, internal_constraints_list =
    get_internal_constraints_list state
  in
  let error, constraints_list =
    List.fold_left
      (fun (error, constraints_list) (domain_name, lemma_list) ->
        let error, current_list =
          List.fold_left (fun (error, current_list) lem ->
              let hyp = Remanent_state.get_hyp lem in
              let refine = Remanent_state.get_refinement lem in
              let string_version =
                Ckappa_backend.Ckappa_backend.get_string_version
                  hyp
              in
              let error, site_graph =
                Ckappa_site_graph.site_graph_to_list error
                  string_version
              in
              let error, refinement =
                Ckappa_site_graph.site_graph_list_to_list error
                  refine
              in
              let lemma =
                {Remanent_state.hyp = site_graph;
                 Remanent_state.refinement = refinement}
              in
              let current_list = lemma :: current_list in
              error, current_list
            ) (error, []) lemma_list
        in
        (*------------------------------------------------------*)
        let pair_list =
          (domain_name, List.rev current_list) :: constraints_list
        in
        error, pair_list
      ) (error, []) internal_constraints_list
  in
  let state =
    Remanent_state.set_constraints_list constraints_list state in
  let state = Remanent_state.set_errors error state in
  state, constraints_list

  let get_constraints_list =
    get_gen
      ~do_we_show_title:title_only_in_kasa
      ~log_title:"translate refinement lemmas"
      Remanent_state.get_constraints_list
      compute_constraints_list

(******************************************************************)

let output_internal_constraints_list ?logger state =
  let state, constraints_list = get_internal_constraints_list state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let state, kappa_handler = get_handler state in
  (*PRINT*)
  let error =
    Ckappa_site_graph.print_internal_pattern
      ?logger parameters error
      kappa_handler
      constraints_list
  in
  let state = Remanent_state.set_errors error state in
  state

let get_constraints_list_to_json state =
  let state, constraints_list =
    get_constraints_list state
  in
    state,
    Remanent_state.lemmas_list_to_json constraints_list

(*********************************************************)
(*symmetries*)

let compute_symmetries
    ?accuracy_level:(accuracy_level=Public_data.Low)
    _show_title state =
  let state, env = get_env state in
  let state, init = get_init state in
  let state, contact_map_int = get_contact_map_int state in
  match env, init, contact_map_int with
  | None, _, _ | _, None, _ | _, _, None  -> state, None
  | Some env, Some init, Some contact_map_int ->
    begin
      let rules =
        Model.fold_rules (fun _ acc r -> r :: acc) [] env
      in
      let parameters = get_parameters state in
      let cache = LKappa_auto.init_cache () in
      let cc_cache = Pattern.PreEnv.of_env (Model.domain env) in
      let _cc_cache, chemical_species =
        Ode_interface.species_of_initial_state_env
          env
          contact_map_int
          cc_cache
          init
      in
      (*let lkappa_rule_list =
        List.fold_left (fun current_list species ->
            let lkappa = Symmetries.species_to_lkappa_rule
                parameters env species
            in
            lkappa :: current_list
          ) [] chemical_species
      in*)
      let state, contact_map =
        get_contact_map ~accuracy_level state
      in
      let rate_convention =
        Remanent_parameters.get_rate_convention parameters
      in
      let _cache, symmetries =
        Symmetries.detect_symmetries
          parameters
          env
          cache
          rate_convention
          chemical_species
          rules
          contact_map
      in
      state, Some symmetries
    end

let get_symmetric_sites
    ?accuracy_level:(accuracy_level=Public_data.Low) =
  get_gen
    (Remanent_state.get_symmetries accuracy_level)
    (compute_symmetries ~accuracy_level )

let output_symmetries
    ?logger
    ?accuracy_level:(accuracy_level=Public_data.Low)
    state =
  let parameters = Remanent_state.get_parameters state in
  let parameters =
    match logger with
    | None -> parameters
    | Some logger -> Remanent_parameters.set_logger parameters logger
  in
  let state, sym = get_symmetric_sites ~accuracy_level state in
  let state, env = get_env state in
  match sym, env with
  | None, _ | _, None -> state
  | Some sym, Some env ->
    let () = Symmetries.print_symmetries parameters env sym in
    state

let get_data = Remanent_state.get_data
  end
