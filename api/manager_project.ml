open Lwt

let stop_simulation (system_process:Kappa_facade.system_process) :
  Api_environment.simulation option -> unit Api.result Lwt.t =
  function
  | None -> Lwt.return (Api_common.result_ok ())
  | Some current ->
    let t : Kappa_facade.t = current#get_runtime_state () in
    (Kappa_facade.stop ~system_process:system_process ~t:t) >>=
    (Result_util.map
       ~ok:(fun _ -> Lwt.return (Api_common.result_ok ()))
       ~error:(fun errors ->
           Lwt.return (Api_common.result_messages errors)))

let to_project project_id (project : Api_environment.project) =
  { Api_types_j.project_id ;
    Api_types_j.project_version = project#get_version () ; }

class manager_project
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_project =
object
  method project_get
      (project_id : Api_types_j.project_id) :
    Api_types_j.project Api.result Lwt.t =
    Lwt.return (Api_common.result_ok (to_project project_id project))

  method project_parse :
    Api_types_j.project_parse Api.result Lwt.t =
    (project#get_state () >>= function
      | Some x -> Lwt.return x
      | None ->
        let harakiri,_ = Lwt.task () in
        let cand =
          Lwt.pick [
            Kappa_facade.parse
              ~system_process
              ~kappa_files:(project#get_files ());
            harakiri >>= fun () ->
            Lwt.return (Result_util.error
                          [Api_common.error_msg
                             "Parse cancelled by modified files"])
          ] in
             let _ = project#set_state cand in
        cand)
    >>=
    (fun state ->
       Lwt.return
         (Result_util.map
            ~ok:(fun kappa_facade ->
                Api_common.result_ok
                  { Api_types_t.project_parse_contact_map =
                      Kappa_facade.get_contact_map kappa_facade;
                    Api_types_t.project_parse_project_version =
                      project#get_version ();
                    Api_types_t.project_parse_raw_ast =
                      Kappa_facade.get_raw_ast kappa_facade;
                  })
            ~error:(fun error -> Api_common.result_messages error)
            state
         )
    )
end
