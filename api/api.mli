(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type manager_code =
  [ `OK | `Accepted | `Created |
    `Bad_request | `Conflict | `Not_found | `Request_timeout ]
type result_code = manager_code
type 'ok result = ('ok,manager_code) Api_types_t.result

type project_id = Api_types_t.project_id

class type manager_environment = object
  method environment_info:
    unit -> Api_types_j.environment_info result Lwt.t
end

class type manager_project = object
  method project_get :
    project_id -> Api_types_j.project result Lwt.t
  method project_parse : Api_types_j.project_parse result Lwt.t
end

class type manager_file = object
  method file_catalog : Api_types_j.file_catalog result Lwt.t

  method file_create :
    Api_types_j.file -> Api_types_j.file_metadata result Lwt.t

  method file_get : Api_types_j.file_id -> Api_types_j.file result Lwt.t

  method file_update :
    Api_types_j.file_id ->
    Api_types_j.file_modification ->
    Api_types_j.file_metadata result Lwt.t

  method file_delete : Api_types_j.file_id -> unit result Lwt.t
end

class type manager_file_line = object
  method simulation_catalog_file_line :
    Api_types_j.file_line_catalog result Lwt.t
  method simulation_detail_file_line :
    Api_types_j.file_line_id -> Api_types_j.file_line list result Lwt.t
end

class type manager_flux_map = object
  method simulation_catalog_flux_map : Api_types_j.flux_map_catalog result Lwt.t
  method simulation_detail_flux_map :
    Api_types_j.flux_map_id -> Api_types_j.flux_map result Lwt.t
end

class type manager_log_message = object
  method simulation_detail_log_message :
    Api_types_j.log_message result Lwt.t
end

class type manager_plot = object
  method simulation_detail_plot :
    Api_types_j.plot_parameter -> Api_types_j.plot_detail result Lwt.t
end

class type manager_snapshot = object
  method simulation_catalog_snapshot : Api_types_j.snapshot_catalog result Lwt.t
  method simulation_detail_snapshot :
    Api_types_j.snapshot_id -> Api_types_j.snapshot result Lwt.t
end

class type manager_simulation = object
  method simulation_delete : unit result Lwt.t

  method simulation_start :
    Api_types_j.simulation_parameter ->
    Api_types_j.simulation_artifact result Lwt.t

  method simulation_pause : unit result Lwt.t

  method simulation_perturbation :
    Api_types_j.simulation_perturbation -> unit result Lwt.t

  method simulation_continue :
    Api_types_j.simulation_parameter -> unit result Lwt.t

  method simulation_info : Api_types_j.simulation_info result Lwt.t

  method simulation_efficiency : Counter.Efficiency.t result Lwt.t

  method simulation_parameter : Api_types_j.simulation_parameter result Lwt.t

  method simulation_raw_trace : string result Lwt.t

  inherit manager_file_line
  inherit manager_flux_map
  inherit manager_log_message
  inherit manager_plot
  inherit manager_snapshot
end

class type virtual manager_static_analysis = object
  method virtual is_running : bool
  method init_static_analyser :
    Ast.parsing_compil -> (unit, string) Lwt_result.t
  method init_static_analyser_raw :
    string -> (unit, string) Lwt_result.t
  (** The string has to be the json corresponding to an [Ast.parsing_compil] *)

  method get_contact_map :
    Public_data.accuracy_level option -> (Yojson.Basic.json,string) Lwt_result.t
  method get_influence_map :
    Public_data.accuracy_level option -> (Yojson.Basic.json,string) Lwt_result.t
  method get_dead_rules : (Yojson.Basic.json,string) Lwt_result.t
  method get_constraints_list : (Yojson.Basic.json,string) Lwt_result.t
end

class type manager = object
  inherit manager_project
  inherit manager_file
  inherit manager_simulation
end

class type concrete_manager = object
  inherit manager
  inherit manager_static_analysis
  method is_running : bool
  method terminate : unit
end

class type rest_manager = object
  inherit manager_environment
  inherit concrete_manager
  method project_catalog : Api_types_j.project_catalog result Lwt.t
  method project_create :
    Api_types_j.project_parameter -> unit result Lwt.t
  method project_delete : project_id -> unit result Lwt.t
end
