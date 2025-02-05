module type Site_graph =
sig
  type t

  type agent_id
  type bond_index

  type binding_state =
    | Free | Wildcard | Bound_to_unknown
    | Binding_type of Ckappa_sig.agent_name * Ckappa_sig.site_name
    | Bound_to of bond_index

  val binding_state_to_json : binding_state -> Yojson.Basic.json
  val binding_state_of_json : Yojson.Basic.json -> binding_state


  val int_of_bond_index : bond_index -> int
  val bond_index_of_int : int -> bond_index

  val empty: t

  val get_string_version : t ->
    (string *
     (string option * binding_state option)
       Wrapped_modules.LoggedStringMap.t)
      Ckappa_sig.Agent_id_map_and_set.Map.t

  val set_string_version : (*FIXME*)
    (string *
     (string option * binding_state option) Wrapped_modules.LoggedStringMap.t)
      Ckappa_sig.Agent_id_map_and_set.Map.t -> t -> t

  val add_agent:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler  ->
    Cckappa_sig.kappa_handler ->
    Ckappa_sig.c_agent_name -> t ->
    Exception.method_handler * agent_id * t

  val add_state:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_state -> t ->
    Exception.method_handler * t

  val add_bound_to_unknown:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name -> t ->
    Exception.method_handler * t

  val add_bond:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val add_bond_type:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    agent_id ->
    Ckappa_sig.c_site_name ->
    Ckappa_sig.c_agent_name ->
    Ckappa_sig.c_site_name ->
    t ->
    Exception.method_handler * t

  val print:
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t -> Exception.method_handler

  val print_aux:
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    string ->
    (string option * binding_state option)
      Wrapped_modules.LoggedStringMap.t -> bool -> bool

  val print_list:
    Loggers.t ->
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    t list -> Exception.method_handler

end
