(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let refresh =
  Api_common.result_bind_lwt
    ~ok:(fun () ->
        State_file.sync ~reset:true () >>=
        fun r -> State_simulation.refresh () >>=
        fun r' -> Lwt.return (Api_common.result_combine [r; r']))

let create_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.create_project project_id >>= refresh) >>=
       fun _ -> Lwt.return_unit)

let set_project (project_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.set_project project_id >>= refresh) >>=
       fun _ -> Lwt.return_unit)

let close_project project_id : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_project.remove_project project_id >>= refresh) >>=
       fun _ -> Lwt.return_unit)

let set_manager (runtime_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (Api_common.result_bind_lwt
            ~ok:(fun () -> Lwt.return (Api_common.result_ok ()))
            (State_runtime.create_spec ~load:true runtime_id)) >>=
       (fun _ -> Lwt.return_unit)
    )
