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

(** State of the running simulation.
*)
type t

val clone_t : t -> t

(** Trivial implementation *)
class null_process : system_process

val parse :
  system_process:system_process ->
  kappa_files:Api_types_t.file list ->
  (t, Api_types_j.errors) Result.result Lwt.t

val start :
  system_process:system_process ->
  parameter:Api_types_j.simulation_parameter ->
  t:t ->
  (unit, Api_types_j.errors) Result.result Lwt.t

val pause :
  system_process:system_process ->
  t:t -> (unit, Api_types_j.errors) Result.result Lwt.t

val stop :
  system_process:system_process ->
  t:t -> (unit, Api_types_j.errors) Result.result Lwt.t

val perturbation :
  system_process:system_process ->
  t:t ->
  perturbation:Api_types_j.simulation_perturbation ->
  (unit, Api_types_j.errors) Result.result Lwt.t

val continue :
  system_process:system_process ->
  t:t ->
  parameter:Api_types_j.simulation_parameter ->
  (unit, Api_types_j.errors) Result.result Lwt.t


val info :
  system_process:system_process ->
  t:t ->
  (Api_types_j.simulation_detail, Api_types_j.errors)
    Result.result Lwt.t

val efficiency : t -> Counter.Efficiency.t

val get_contact_map : t -> Api_types_j.site_node array

val get_dead_rules : t -> string list
