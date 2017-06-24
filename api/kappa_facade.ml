open Lwt.Infix

(** Interface to kappa runtime *)
(* Error messages *)
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_perturbation_context =
  "Invalid runtime state missing missing perturbation context"

(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process =
  object
    method log : ?exn:exn -> string -> unit Lwt.t
    method yield : unit -> unit Lwt.t
    method min_run_duration : unit -> float
  end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) =
      let () = ignore(exn) in
      Lwt.return_unit
    method yield () = Lwt.return_unit
    method min_run_duration() = 0.0
  end;;

type file_index =
  { file_index_file_id : Api_types_j.file_id ;
    file_index_line_offset : int ;
    file_index_char_offset : int ;
    file_line_count : int ; }

type kappa_file =
  { kappa_file_id : Api_types_j.file_id ;
    kappa_file_code : string ;
  }

type kappa_code = kappa_file list

(** State of the running simulation. *)
type t =
  { mutable is_running : bool ;
    mutable run_finalize : bool ;
    mutable pause_condition : (Pattern.id array list,int) Alg_expr.bool ;
    new_syntax : bool;
    dumpIfDeadlocked : bool;
    maxConsecutiveClash : int;
    counter : Counter.t ;
    log_buffer : Buffer.t ;
    log_form : Format.formatter ;
    mutable plot : Api_types_j.plot ;
    mutable snapshots : Api_types_j.snapshot list ;
    mutable flux_maps : Api_types_j.flux_map list ;
    mutable species : (float*Raw_mixture.t) list Mods.StringMap.t;
    mutable files : Api_types_j.file_line list ;
    mutable error_messages : Api_types_j.errors ;
    mutable trace : Trace.t ;
    ast : Ast.parsing_compil;
    contact_map : Contact_map.t ;
    mutable env : Model.t ;
    mutable graph : Rule_interpreter.t ;
    mutable state : State_interpreter.t ;
    init_l : (Alg_expr.t * Primitives.elementary_rule * Locality.t) list ;
    mutable lastyield : float ;
  }

let create_t ~log_form ~log_buffer ~contact_map ~new_syntax
    ~dumpIfDeadlocked ~maxConsecutiveClash ~env ~counter ~graph
    ~state ~init_l ~lastyield ~ast : t = {
  is_running = false; run_finalize = false; counter; log_buffer; log_form;
  pause_condition = Alg_expr.FALSE; dumpIfDeadlocked; maxConsecutiveClash;
  new_syntax;
  plot = { Api_types_j.plot_legend = [] ;
           Api_types_j.plot_time_series = [] ; } ;
  snapshots = [];
  flux_maps = [];
  species = Mods.StringMap.empty;
  files = [];
  error_messages = [];
  trace = [];
  ast; contact_map; env; graph; state; init_l;
  lastyield;
}

let reinitialize random_state t =
  let () = Counter.reinitialize t.counter in
(*  let () = Format.pp_print_flush t.log_form () in
    let () = Buffer.reset t.log_buffer in*)
  t.is_running <- false;
  t.run_finalize <- false;
  t.pause_condition <- Alg_expr.FALSE;
  t.plot <- { Api_types_j.plot_legend = [] ;
              Api_types_j.plot_time_series = [] ; } ;
  t.snapshots <- [];
  t.flux_maps <- [];
  t.files <- [];
  t.error_messages <- [];
  t.graph <- Rule_interpreter.empty
      ~with_trace:false
      random_state t.env t.counter;
  t.state <- State_interpreter.empty ~with_delta_activities:false t.env []

let clone_t t =
  create_t
    ~log_form:t.log_form ~log_buffer:t.log_buffer
    (* TODO pirbo: Should I create a new buffer? *)
    ~contact_map:t.contact_map
    ~new_syntax:t.new_syntax
    ~maxConsecutiveClash:t.maxConsecutiveClash
    ~dumpIfDeadlocked:t.dumpIfDeadlocked
    ~ast:t.ast ~env:t.env
    ~counter:t.counter (* FALSE imperatively modified *)
    ~graph:t.graph (* FALSE imperatively modified *)
    ~state:t.state (* FALSE imperatively modified *)
    ~init_l:t.init_l
    ~lastyield:t.lastyield



let catch_error : 'a . (Api_types_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
    (function
      |  ExceptionDefn.Syntax_Error ((message,region) : string Locality.annot) ->
        handler (Api_data.api_message_errors ~region message)
      | ExceptionDefn.Malformed_Decl ((message,region) : string Locality.annot) ->
        handler (Api_data.api_message_errors ~region message)
      | ExceptionDefn.Internal_Error ((message,region) : string Locality.annot) ->
        handler (Api_data.api_message_errors ~region message)
      | Invalid_argument error ->
        handler (Api_data.api_message_errors ("Runtime error "^ error))
      | exn -> handler (Api_data.api_exception_errors exn))

type file = { file_id : string ; file_content : string }

let rec compile_file
    (yield : unit -> unit Lwt.t)
    (compile : Ast.parsing_compil) : file list -> (Ast.parsing_compil, Api_types_j.errors) Result.result Lwt.t =
  function
  | [] -> Lwt.return (Result_util.ok compile)
  | file::files ->
    let lexbuf = Lexing.from_string file.file_content in
    let () = lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file.file_id }  in
    Lwt.catch
      (fun () ->
         (Lwt.wrap3 KappaParser.start_rule KappaLexer.token lexbuf compile) >>=
         (fun new_compile ->
            (yield ()) >>=
            (fun () -> compile_file yield new_compile files))
      )
      (catch_error
         (fun e ->
            (yield ()) >>=
            (fun () -> compile_file yield compile files) >>=
            (fun result ->
               let r =
                 Result_util.map
                   ~ok:(fun _ -> Result_util.error e)
                   ~error:(fun error -> Result_util.error (e@error))
                   result
                in
                (Lwt.return r)
            )
         )
      )

let build_ast (kappa_files : file list) (yield : unit -> unit Lwt.t) =
  let log_buffer = Buffer.create 512 in
  let log_form = Format.formatter_of_buffer log_buffer in
  let post_parse ast =
    let (conf,_,_,_,_) =
      Configuration.parse ast.Ast.configurations in
    let new_syntax = conf.Configuration.newSyntax in
    (Lwt.wrap2 (LKappa.compil_of_ast ~new_syntax) [] ast) >>=
    (fun
      (sig_nd,
       contact_map,
       tk_nd,_algs_nd,
       _updated_vars,
       (result :
          (Ast.agent,
           LKappa.rule_agent list,
           int,
           LKappa.rule_agent LKappa.rule, unit) Ast.compil)) ->
      (yield ()) >>=
      (fun () ->
         (* The last yield is updated after the last yield.
            It is gotten here for the initial last yeild value. *)
         let lastyield = Sys.time () in
         try (* exception raised by compile must have used Lwt.fail.
                Something is wrong for now *)
           Eval.compile
             ~pause:(fun f -> Lwt.bind (yield ()) f)
             ~return:Lwt.return ?rescale_init:None ~compileModeOn:false
             ~outputs:(function
                 | Data.Log s ->
                   Format.fprintf log_form "%s@." s
                 | Data.Snapshot _
                 | Data.Flux _
                 | Data.Species _
                 | Data.DeltaActivities _
                 | Data.Plot _
                 | Data.TraceStep _
                 | Data.Print _ -> assert false)
             ~max_sharing:false sig_nd tk_nd contact_map result >>=
           (fun (env,with_trace,init_l) ->
              let counter =
                Counter.create
                  ~init_t:(0. : float) ~init_e:(0 : int)
                  ?max_time:None ?max_event:None
                  ~plot_period:(Counter.DT 1.) in
              let () = ExceptionDefn.flush_warning log_form in
              let random_state =
                match conf.Configuration.seed with
                | None -> Random.State.make_self_init ()
                | Some theSeed -> Random.State.make [|theSeed|] in
              let simulation =
                create_t
                  ~contact_map ~log_form ~log_buffer ~ast ~env ~counter
                  ~dumpIfDeadlocked:conf.Configuration.dumpIfDeadlocked
                  ~maxConsecutiveClash:conf.Configuration.maxConsecutiveClash
                  ~new_syntax
                  ~graph:(Rule_interpreter.empty
                            ~with_trace
                            random_state env counter)
                  ~state:(State_interpreter.empty ~with_delta_activities:false env [])
                  ~init_l ~lastyield
              in
              Lwt.return (Result_util.ok simulation))
         with e ->
           (catch_error
              (fun e -> Lwt.return (Result_util.error e))) e
      ))
  in
  Lwt.catch
    (fun () ->
       (compile_file yield Ast.empty_compil kappa_files) >>=
       (Result_util.map
          ~ok:(fun raw_ast ->
              (yield ()) >>=
              (fun () -> post_parse raw_ast))
         ~error:(fun e -> Lwt.return (Result_util.error e))
       )
    )
    (catch_error (fun e -> Lwt.return (Result_util.error e)))

let prepare_plot_value x =
  Array.fold_right (fun nbr acc -> Nbr.to_float nbr :: acc) x []

let outputs (simulation : t) =
  function
  | Data.Flux flux_map ->
    simulation.flux_maps <- flux_map::simulation.flux_maps
  | Data.DeltaActivities _ -> assert false
  | Data.Plot new_observables ->
    let new_values = prepare_plot_value new_observables in
    simulation.plot <-
      {simulation.plot with
       Api_types_j.plot_time_series =
         new_values :: simulation.plot.Api_types_j.plot_time_series }
  | Data.Species(file,time,mix) ->
    let p = Mods.StringMap.find_default [] file simulation.species in
    simulation.species <-
      Mods.StringMap.add file ((time,mix)::p) simulation.species
  | Data.Print file_line ->
    simulation.files <- file_line::simulation.files
  | Data.Snapshot snapshot ->
    simulation.snapshots <-
      (Api_data.label_snapshot
         (Model.signatures simulation.env)
         snapshot)::simulation.snapshots
  | Data.Log s -> Format.fprintf simulation.log_form "%s@." s
  | Data.TraceStep st -> simulation.trace <- st :: simulation.trace

let parse
    ~(system_process : system_process)
    ~(kappa_files : Api_types_t.file list)
  : (t,Api_types_j.errors) Result.result Lwt.t
  =

  let kappa_files =
    List.fold_left
      (fun acc f ->
         if f.Api_types_t.file_metadata.Api_types_t.file_metadata_compile
         then {
           file_id = f.Api_types_t.file_metadata.Api_types_t.file_metadata_id ;
           file_content = f.Api_types_t.file_content
         }::acc
         else acc)
         [] kappa_files in
  Lwt.bind
    (build_ast
       kappa_files system_process#yield)
    (Result_util.map
       ~ok:(fun simulation -> Lwt.return (Result_util.ok simulation))
       ~error:(fun e -> Lwt.return (Result_util.error e)))

let time_yield
    ~(system_process : system_process)
    ~(t : t) : unit Lwt.t =
  let time = Sys.time () in
  if time -. t.lastyield > system_process#min_run_duration () then
    let () = t.lastyield <- time in
    system_process#yield ()
  else Lwt.return_unit

let finalize_simulation ~(t : t) : unit =
  State_interpreter.end_of_simulation
    ~outputs:(outputs t) t.log_form t.env t.counter t.graph t.state

let run_simulation
    ~(system_process : system_process) ~(t : t) stopped : unit Lwt.t =
  Lwt.catch
    (fun () ->
       let rstop = ref stopped in
       let () = t.is_running <- true in
       let rec iter () =
         (try
            let () =
              while (not !rstop) &&
                    Sys.time () -. t.lastyield <
                    system_process#min_run_duration ()
              do
                let (stop,graph',state') =
                  State_interpreter.a_loop
                    ~outputs:(outputs t) ~dumpIfDeadlocked:t.dumpIfDeadlocked
                    ~maxConsecutiveClash:t.maxConsecutiveClash
                    t.env t.counter t.graph t.state in
                rstop := stop || Rule_interpreter.value_bool
                           t.counter graph' t.pause_condition;
                t.graph <- graph';
                t.state <- state'
              done in
            Lwt.return_unit
          with e -> Lwt.fail e) >>= fun () ->
         if !rstop then
           let () = t.is_running <- false in
           Lwt.return_unit
         else if t.is_running then
           (system_process#yield ()) >>= (fun () ->
               let () = t.lastyield <- Sys.time () in iter ())
         else
           Lwt.return_unit in
       (iter ()) >>=
       (fun () ->
          let () =
            if t.run_finalize then
              finalize_simulation ~t:t
            else
              ExceptionDefn.flush_warning t.log_form
          in
          Lwt.return_unit))
    (catch_error
       (fun e ->
          let () = t.is_running <- false in
          let () = t.error_messages <- e in
          Lwt.return_unit))

let start
    ~(system_process : system_process)
    ~(parameter : Api_types_j.simulation_parameter)
    ~(t : t)
  : (unit,Api_types_j.errors) Result.result Lwt.t =
  let lexbuf =
    Lexing.from_string parameter.Api_types_j.simulation_pause_condition in
  Lwt.catch (fun () ->
      (*let () =
          Counter.set_max_time
                 t.counter
                 parameter.Api_types_j.simulation_max_time
               in
               let () =
               Counter.set_max_events
                 t.counter
                 parameter.Api_types_j.simulation_max_events
               in*)
      let random_state =
        match parameter.Api_types_j.simulation_seed with
        | None -> Random.State.make_self_init ()
        | Some seed -> Random.State.make [|seed|] in
      let () = reinitialize random_state t in
      Lwt.wrap2 KappaParser.standalone_bool_expr KappaLexer.token lexbuf >>=
      fun pause ->
      Lwt.wrap4 (Evaluator.get_pause_criteria
                   ~max_sharing:false ~new_syntax:t.new_syntax)
        t.contact_map t.env t.graph pause >>=
      fun (env',graph',b'') ->
      let () = t.env <- env' in
      let () = t.graph <- graph' in
      let () = t.pause_condition <- b'' in
      let () =
        Counter.set_plot_period
          t.counter
          (Counter.DT parameter.Api_types_j.simulation_plot_period) in
      let () =
        Lwt.async
          (fun () ->
             try (* exception raised by build_initial_state must have been
                    raised with Lwt.fail. Something is wrong for now... *)
               Eval.build_initial_state
                 ~bind:(fun x f ->
                     (time_yield ~system_process:system_process ~t:t) >>=
                     (fun () -> x >>= f))
                 ~return:Lwt.return ~outputs:(outputs t)
                 ~with_trace:parameter.Api_types_t.simulation_store_trace
                 ~with_delta_activities:false
                 t.counter
                 t.env
                 random_state
                 t.init_l >>=
               (fun (stop,graph,state) ->
                  let () = t.graph <- graph; t.state <- state in
                  let () = ExceptionDefn.flush_warning t.log_form in
                  let legend =
                    Model.map_observables
                      (fun o -> Format.asprintf
                          "%a"
                          (Kappa_printer.alg_expr
                             ~env:t.env) o)
                      t.env in
                  let first_obs =
                    State_interpreter.observables_values
                      t.env graph t.counter in
                  let first_values = prepare_plot_value first_obs in
                  let () =
                    t.plot <-
                      { Api_types_j.plot_legend = Array.to_list legend;
                        Api_types_j.plot_time_series = [ first_values ]} in
                  run_simulation ~system_process:system_process ~t:t stop)
             with e ->
               catch_error
                 (fun e ->
                    let () = t.error_messages <- e in
                    Lwt.return_unit) e
          ) in
      Lwt.return (Result_util.ok ()))
    (catch_error
       (fun e ->
          let () = t.error_messages <- e in
          Lwt.return (Result_util.error e)))

let pause
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  let () = if t.is_running then
      t.is_running <- false
    else
      ()
  in
  Lwt.return (Result_util.ok ())

let stop
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  Lwt.catch
    (fun () ->
       let () = t.run_finalize <- true in
       (if t.is_running then
          pause ~system_process:system_process ~t:t
        else
          let () = finalize_simulation ~t:t in
          Lwt.return (Result_util.ok ()))
    )
    (catch_error (fun e -> Lwt.return (Result_util.error e)))

let perturbation
    ~(system_process : system_process)
    ~(t : t)
    ~(perturbation:Api_types_j.simulation_perturbation)
  : (unit, Api_types_j.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let lexbuf =
    Lexing.from_string perturbation.Api_types_j.perturbation_code
  in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt.return
           (Result_util.error (Api_data.api_message_errors msg_process_not_paused))
       else
         Lwt.wrap2
           KappaParser.standalone_effect_list KappaLexer.token lexbuf >>=
         fun e ->
         Lwt.wrap6
           (Evaluator.do_interactive_directives
              ~outputs:(outputs t) ~max_sharing:false ~new_syntax:t.new_syntax)
           t.contact_map t.env t.counter t.graph t.state e >>=
         fun (_,(env',(_,graph'',state'))) ->
         let () = t.env <- env' in
         let () = t.graph <- graph'' in
         let () = t.state <- state' in
         Lwt.return (Result_util.ok ()))
    (catch_error (fun e -> Lwt.return (Result_util.error e)))

let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(parameter : Api_types_j.simulation_parameter)
  : (unit,Api_types_j.errors) Result.result Lwt.t =
  let lexbuf =
    Lexing.from_string parameter.Api_types_j.simulation_pause_condition in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt.return (Result_util.ok ())
       else
         Lwt.wrap2 KappaParser.standalone_bool_expr KappaLexer.token lexbuf >>=
         fun pause ->
         Lwt.wrap4 (Evaluator.get_pause_criteria
                      ~max_sharing:false ~new_syntax:t.new_syntax)
           t.contact_map t.env t.graph pause >>=
         fun (env',graph',b'') ->
         let () = t.env <- env' in
         let ()  = t.graph <- graph' in
         let () = t.pause_condition <- b'' in
         (*let () =
           Counter.set_max_time
             t.counter
             parameter.Api_types_j.simulation_max_time
         in
         let () =
           Counter.set_max_events
             t.counter
             parameter.Api_types_j.simulation_max_events
           in*)
         let () =
           Lwt.async
             (fun () ->
                run_simulation ~system_process:system_process ~t:t false) in
         Lwt.return (Result_util.ok ())
    )
    (catch_error
       (fun e -> Lwt.return (Result_util.error e)))

let create_info ~(t : t) : Api_types_j.simulation_detail =
  let progress :  Api_types_j.simulation_progress =
    { Api_types_j.simulation_progress_time =
        Counter.current_time t.counter ;
      Api_types_j.simulation_progress_time_percentage =
        Counter.time_percentage t.counter ;
      Api_types_j.simulation_progress_event =
        Counter.current_event t.counter ;
      Api_types_j.simulation_progress_event_percentage =
        Counter.event_percentage t.counter ;
      Api_types_j.simulation_progress_tracked_events =
        Counter.tracked_events t.counter ;
      Api_types_j.simulation_progress_is_running =
        t.is_running ;
    } in
  let output : Api_types_j.simulation_detail_output =
    { Api_types_j.simulation_output_plot =
        Some t.plot ;
      Api_types_j.simulation_output_flux_maps =
        t.flux_maps ;
      Api_types_j.simulation_output_file_lines =
        t.files ;
      Api_types_j.simulation_output_snapshots =
        t.snapshots ;
      Api_types_j.simulation_output_log_messages =
        Buffer.contents t.log_buffer ; }
  in
  { Api_types_j.simulation_detail_progress =
      progress ;
    Api_types_j.simulation_detail_output =
      output ; }

let info
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_j.simulation_detail,Api_types_j.errors) Result.result Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
         Lwt.return (Result_util.ok (create_info ~t:t)))
      (catch_error (fun e -> Lwt.return (Result_util.error e)))
  | _ -> Lwt.return (Result_util.error t.error_messages)

let efficiency t = Counter.get_efficiency t.counter

let get_raw_trace t =
  Yojson.Basic.to_string
    (`Assoc [
        "env", Model.to_yojson t.env;
        "trace", `List (List.rev_map Trace.step_to_yojson t.trace);
      ])

let get_raw_ast t =
  Yojson.Basic.to_string (Ast.compil_to_json t.ast)

let get_contact_map (t : t) : Api_types_j.site_node array =
  Api_data.api_contact_map
    (Model.signatures t.env)
    t.contact_map
