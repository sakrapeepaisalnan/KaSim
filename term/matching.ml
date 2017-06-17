(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = Renaming.t Mods.IntMap.t * Mods.IntSet.t
(* (map,set)
   map: point_i -> (node_j(i) -> id_node_graph_in_current_matching)
   set:codomain of current matching *)

type matching = t

let empty = (Mods.IntMap.empty, Mods.IntSet.empty)

let add_cc (inj,co) id r =
  let c = Renaming.image r in
  match Mods.IntSet.disjoint_union co c with
  | Some co' -> Some (Mods.IntMap.add id r inj, co')
  | None -> None

let debug_print f (m,_co) =
  Format.fprintf
    f "@[(%a)@]"
    (Pp.set Mods.IntMap.bindings Pp.comma
       (fun f (ccid,nm) ->
          Pp.set Renaming.to_list Pp.comma
            (fun f (node,dst) ->
               Format.fprintf f "%i:%i->%i" ccid node dst) f nm)) m

(*- rm - reconstruct: Edges.t -> t -> int -> cc -> int -> t option*)
let reconstruct domain graph inj id cc_id root =
  let point = Pattern.Env.get domain cc_id in
  match Pattern.Env.roots point with
  | None -> failwith "Matching.reconstruct cc error"
  (*- rm - add : int -> int -> Renaming.t -> Renaming.t *)
  | Some (rids,rty) ->
    (* -rm - full_rename: Renaming.t option *)
    let _,full_rename =
      (*- rm - to_navigation: bool -> cc -> list *)
      match Pattern.reconstruction_navigation (Pattern.Env.content point) with
      | _::_ as nav ->
        List.fold_left
          (fun (root,inj_op) nav ->
             match inj_op with
             | None -> None,None
             | Some inj ->
               None,Navigation.injection_for_one_more_edge ?root inj graph nav)
          (Some (root,rty),Some Renaming.empty) nav
      (*- rm - find_root: cc -> (type, node) option *)
      | [] -> None, match rids with
        | [rid] -> Renaming.add rid root Renaming.empty
        | _ -> None in
    match full_rename with
    | None -> failwith "Matching.reconstruct renaming error"
    | Some rename ->
      match Mods.IntSet.disjoint_union (Renaming.image rename) (snd inj) with
      | None -> None
      | Some co -> Some (Mods.IntMap.add id rename (fst inj),co)

let rec aux_is_root_of graph root inj = function
  | [] -> true
  | h :: t ->
    match Navigation.injection_for_one_more_edge ?root inj graph h with
    | None -> false
    | Some inj' -> aux_is_root_of graph None inj' t
let is_root_of domain graph (_,rty as root) cc_id =
  let point = Pattern.Env.get domain cc_id in
  match Pattern.reconstruction_navigation (Pattern.Env.content point) with
  | [] ->
    (match Pattern.Env.roots point with
     | Some (_,rty') -> rty = rty'
     | None -> false)
  | nav -> aux_is_root_of graph (Some root) Renaming.empty nav

let roots_of domain graph cc =
  Edges.all_agents_where (fun x -> is_root_of domain graph x cc) graph

(* get : (ContentAgent.t * int) -> t -> int *)
let get ((node,_),id) (t,_) =
  Renaming.apply (Mods.IntMap.find_default Renaming.empty id t) node

let elements_with_types domain ccs (t,_) =
  let out = Array.make (Mods.IntMap.size t) [] in
  let () =
    Mods.IntMap.iter
      (fun id map ->
         out.(id) <- Renaming.fold
             (fun i out acc ->
                (out,Pattern.find_ty
                   (Pattern.Env.content (Pattern.Env.get domain ccs.(id))) i)::acc)
               map [])
        t in
    out

module Cache = struct
  type t = Pattern.id * (int * int) option
  let compare (a,a') (b,b') =
    let c = Pattern.compare_canonicals a b in
    if c = 0 then
      match a',b' with
      | None, None -> 0
      | None,Some _ -> 1
      | Some _, None -> -1
      | Some x, Some y -> Mods.int_pair_compare x y
    else c
  let print f (a,a') =
    Format.fprintf f "%a%a"
      (Pattern.print ~new_syntax:true ?dotnet:None ?domain:None ~with_id:true) a
      (Pp.option (Pp.pair Format.pp_print_int Format.pp_print_int)) a'
end
module CacheSetMap = SetMap.Make(Cache)

type cache = CacheSetMap.Set.t
let empty_cache = CacheSetMap.Set.empty

let survive_nav inj graph =
  List.fold_left
    (fun inj step ->
       match inj with
       | None -> inj
       | Some inj ->
         Navigation.injection_for_one_more_edge inj graph step)
    (Some inj)

(*edges: list of concrete edges,
    returns the roots of observables that are above in the domain*)
let from_edge domain graph (out,cache as acc) edge =
  let rec aux_from_edges cache (obs,rev_deps as acc) = function
    | [] -> acc,cache
    | (pid,point,inj_point2graph) :: remains ->
      let acc' =
        match Pattern.Env.roots point with
        | None -> acc
        |Some (ids,ty) ->
          (List.fold_left
             (fun acc id ->
                (pid,(Renaming.apply inj_point2graph id,ty))::acc)
             obs ids,
           Operator.DepSet.union rev_deps (Pattern.Env.deps point)) in
      let remains',cache' =
        List.fold_left
          (fun (re,ca as pair) son ->
             match survive_nav inj_point2graph graph son.Pattern.Env.next with
             | None -> pair
             | Some inj' ->
               let rename = Renaming.compose false son.Pattern.Env.inj inj' in
               let ca' = CacheSetMap.Set.add
                   (son.Pattern.Env.dst,Renaming.min_elt rename) ca in
               if ca == ca'
               then pair
               else
                 let p' = Pattern.Env.get domain son.Pattern.Env.dst in
                 let next = (son.Pattern.Env.dst,p',rename) in
                 (next::re,ca'))
          (remains,cache) (Pattern.Env.sons point) in
      aux_from_edges cache' acc' remains' in
  match Pattern.Env.get_elementary domain edge with
  | None -> acc
  | Some x ->
    aux_from_edges
      (*(*unnecessary*)CacheSetMap.Set.add (cc_id,Renaming.min_elt inj')*)
      cache out [x]

let observables_from_agent
    domain graph ((obs,rdeps),cache as acc) (_,ty as node) =
  if Edges.is_agent node graph
  then match Pattern.Env.get_single_agent ty domain with
    | Some (cc,deps) ->
      ((cc,node)::obs,Operator.DepSet.union rdeps deps),cache
    | None -> acc
  else acc

let observables_from_free domain graph acc node site =
  from_edge domain graph acc
    ((Navigation.Fresh node,site),Navigation.ToNothing)
let observables_from_internal domain graph acc node site id =
  from_edge domain graph acc
    ((Navigation.Fresh node,site),Navigation.ToInternal id)
let observables_from_link domain graph acc n site  n' site' =
  from_edge domain graph acc
    ((Navigation.Fresh n,site),
     Navigation.ToNode (Navigation.Fresh n',site'))

module Agent = struct
  type t =
    | Existing of Agent.t * int
    | Fresh of int * int (* type, id *)

  let rename id inj = function
    | Existing (n, id') as x ->
      if id <> id' then x else
        let n' = Agent.rename inj n in
        if n == n' then x else Existing (n',id')
    | Fresh _ as x -> x

  let print ?sigs f = function
    | Existing (n,id) ->
      Format.fprintf
        f "%a/*%i*/" (Agent.print ?sigs ~with_id:true) n id
    | Fresh (ty,i) ->
      Format.fprintf f "%a/*%t %i*/"
        (match sigs with
         | None -> Format.pp_print_int
         | Some sigs -> Signature.print_agent sigs) ty Pp.nu i

  let print_site ?sigs place f site =
    match place with
    | Existing (n,_) -> Agent.print_site ?sigs n f site
    | Fresh (ty,_) ->
      match sigs with
      | None -> Format.pp_print_int f site
      | Some sigs -> Signature.print_site sigs ty f site

  let print_internal ?sigs place site f id =
    match place with
    | Existing (n,_) -> Agent.print_internal ?sigs n site f id
    | Fresh (ty,_) ->
      match sigs with
      | None -> Format.fprintf f "%i~%i" site id
      | Some sigs ->
        Signature.print_site_internal_state sigs ty site f (Some id)

  let get_type = function
    | Existing (n,_) -> Agent.sort n
    | Fresh (i,_) -> i

  let get_id = function
    | Existing (n,_) -> Agent.id n
    | Fresh (_,i) -> i

  let same_connected_component p p' =
    match p,p' with
    | (Existing _, Fresh _ | Fresh _, Existing _) -> false
    | Fresh (_,i), Fresh (_,i') -> i=i'
    | Existing (_,id), Existing (_,id') -> id=id'

  let is_fresh = function
    | Existing _ -> false
    | Fresh _ -> true

  let concretize (inj_nodes,inj_fresh) = function
    | Existing (n,id) -> (get (n,id) inj_nodes,Agent.sort n)
    | Fresh (ty,id) ->
      match Mods.IntMap.find_option id inj_fresh with
      | Some x -> (x,ty)
      | None -> failwith "Instantiation.from_place"

  let to_yojson = function
    | Existing (n,ty) ->
       ((`Assoc ["Existing",
                 (`List [`Assoc ["agent",(Agent.to_json n)];
                         `Assoc ["type",`Int ty]])])
        :Yojson.Basic.json)
    | Fresh (id,ty) -> `Assoc ["Fresh",(`Assoc [ "id",`Int id; "type",`Int ty])]

  let of_yojson = function
      |`Assoc ["Existing",`List list] ->
      (match list with
       | [`Assoc ["agent", a]; `Assoc ["type", `Int ty]] ->
          Existing ((Agent.of_json a),ty)
       | x::_ -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x))
       | [] -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",`Null)))
      |`Assoc ["Fresh",a] ->
      (match a with
       | `Assoc ["id", `Int id; "type", `Int ty] -> Fresh (id,ty)
       | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x)))
    | x -> raise (Yojson.Basic.Util.Type_error ("Invalid agent",x))
end
