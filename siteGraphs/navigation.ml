(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type abstract = Existing of int | Fresh of Agent.t

type 'a port = 'a * int

type 'a arrow = ToNode of 'a port | ToNothing | ToInternal of int

type 'a step = 'a port * 'a arrow

type 'a t = 'a step list

let print_id sigs f = function
  | Existing id -> Format.pp_print_int f id
  | Fresh (id,ty) ->
    Format.fprintf f "!%a-%i" (Signature.print_agent sigs) ty id

let print_id_site ?source sigs find_ty n =
  let ty =
    match n with
    | Fresh (_,ty) -> ty
    | Existing id ->
      match source with
      | Some (Fresh (id',ty)) when id = id' -> ty
      | (None | Some (Fresh _ | Existing _)) -> find_ty id in
  Signature.print_site sigs ty

let print_id_internal_state sigs find_ty n =
  Signature.print_site_internal_state
    sigs (match n with Existing id -> find_ty id | Fresh (_,ty) -> ty)

let id_up_to_alpha_to_yojson = function
  | Existing i -> `Int i
  | Fresh f -> Agent.to_json f
let id_up_to_alpha_of_yojson = function
  | `Int i -> Existing i
  | x -> Fresh (Agent.of_json x)

let port_to_yojson (a,s) = `List [id_up_to_alpha_to_yojson a; `Int s]
let port_of_yojson = function
  | `List [ a; `Int s] -> (id_up_to_alpha_of_yojson a,s)
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect navigation port",x))

let arrow_to_yojson = function
  | ToNode p -> port_to_yojson p
  | ToNothing -> `Null
  | ToInternal i -> `Int i
let arrow_of_yojson = function
  | `Null -> ToNothing
  | `Int i -> ToInternal i
  | x -> ToNode (port_of_yojson x)

let step_to_yojson (p,a) = `List [port_to_yojson p; arrow_to_yojson a]
let step_of_yojson = function
  | `List [ p ; a ] ->
    (port_of_yojson p, arrow_of_yojson a)
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect navigation step",x))

let to_yojson l = `List (List.map step_to_yojson l)
let of_yojson = function
  | `List l -> List.map step_of_yojson l
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect navigation",x))

let extend f = function
  | Existing _ -> f
  | Fresh (id,ty) -> fun x -> if x = id then ty else f x

let rec print sigs find_ty f = function
  | [] -> ()
  | ((source,site), ToNothing) :: t ->
    Format.fprintf f "-%a_%a-%t->%a" (print_id sigs) source
      (print_id_site sigs find_ty source) site Pp.bottom
      (print sigs (extend find_ty source)) t
  | ((source,site), ToNode (id,port)) :: t ->
    Format.fprintf f "-%a_%a-%a_%a->%a" (print_id sigs) source
      (print_id_site sigs find_ty source) site
      (print_id sigs) id
      (print_id_site ~source sigs find_ty id) port
      (print sigs (extend (extend find_ty id) source)) t
  | ((source,site), ToInternal i) :: t ->
    Format.fprintf
      f "-%a_%a->%a" (print_id sigs) source
      (print_id_internal_state sigs find_ty source site) (Some i)
      (print sigs (extend find_ty source)) t

let compatible_point inj e e' =
  match e,e' with
  | ((Existing id,site), ToNothing), e ->
    if Renaming.mem id inj &&
       e = ((Existing (Renaming.apply inj id),site),ToNothing)
    then Some inj
    else None
  | ((Existing id,site), ToInternal i), e ->
    if Renaming.mem id inj &&
       e = ((Existing (Renaming.apply inj id),site),ToInternal i)
    then Some inj
    else None
  | ((Existing id,site), ToNode (Existing id',site')), e ->
    if Renaming.mem id inj && Renaming.mem id' inj &&
       (e =
        ((Existing (Renaming.apply inj id),site),
         ToNode (Existing (Renaming.apply inj id'),site'))
        || e =
           ((Existing (Renaming.apply inj id'),site'),
            ToNode (Existing (Renaming.apply inj id),site)))
    then Some inj
    else None
  | (((Existing id,site),ToNode (Fresh (id',ty),site')),
     ((Existing sid,ssite), ToNode (Fresh(sid',ty'),ssite'))
    | ((Fresh (id',ty),site),ToNode (Existing id,site')),
      ((Existing sid,ssite), ToNode (Fresh(sid',ty'),ssite'))
    | ((Existing id,site),ToNode (Fresh (id',ty),site')),
      ((Fresh(sid',ty'),ssite), ToNode (Existing sid,ssite'))
    | ((Fresh (id',ty),site),ToNode (Existing id,site')),
      ((Fresh(sid',ty'),ssite), ToNode (Existing sid,ssite'))) ->
    if  ssite = site && ty' = ty && ssite' = site'
        && not (Renaming.mem id' inj) then
      match Renaming.add id' sid' inj with
      | Some inj' when Renaming.mem id inj' && sid = Renaming.apply inj' id  ->
        Some inj'
      | _ -> None
   else None
  | ((Existing _,_), ToNode (Fresh _,_)),
    (((Fresh _ | Existing _), _), _) -> None
  | ((Fresh (id,ty),site), ToNothing), ((Fresh (id',ty'),site'),x) ->
    if ty = ty' && site = site' && x = ToNothing
       && not (Renaming.mem id inj)
    then Renaming.add id id' inj
    else None
  | ((Fresh (id,ty),site), ToInternal i), ((Fresh (id',ty'),site'),x) ->
    if ty = ty' && site = site' &&
       x = ToInternal i && not (Renaming.mem id inj)
    then Renaming.add id id' inj
    else None
  | ((Fresh (id,ty),site), ToNode (Fresh (id',ty'),site')),
    ((Fresh (sid,sty),ssite), ToNode (Fresh (sid',sty'),ssite')) ->
    if not (Renaming.mem id inj) && not (Renaming.mem id' inj) then
      if ty = sty && site = ssite && ty' = sty' && site' = ssite'
      then match Renaming.add id sid inj with
        | None -> None
        | Some inj' -> match Renaming.add id' sid' inj' with
          | None -> None
          | Some inj'' -> Some inj''
      else if ty = sty' && site = ssite' && ty' = sty && site' = ssite
      then match Renaming.add id sid' inj with
        | None -> None
        | Some inj' -> match Renaming.add id' sid inj' with
          | None -> None
          | Some inj'' -> Some inj''
      else None
    else None
  | ((Fresh _,_), _), ((Fresh _,_),_) -> None
  | ((Fresh _,_), _), ((Existing _,_),_) -> None

let rec aux_sub inj goal acc = function
  | [] -> None
  | h :: t -> match compatible_point inj h goal with
    | None -> aux_sub inj goal (h::acc) t
    | Some inj' -> Some (inj',List.rev_append acc t)
let rec is_subnavigation inj nav = function
  | [] -> Some (inj,nav)
  | h :: t -> match aux_sub inj h [] nav with
    | None -> None
    | Some (inj',nav') -> is_subnavigation inj' nav' t

let rename_id inj2cc = function
  | Existing n -> inj2cc,Existing (Renaming.apply inj2cc n)
  | Fresh (id,ty) ->
    let img = Renaming.image inj2cc in
    let id' =
      if Mods.IntSet.mem id img then
        match Mods.IntSet.max_elt img with
        | None -> 1
        | Some i -> succ i
    else id in
    match Renaming.add id id' inj2cc with
    | None -> assert false
    | Some inj' -> inj',Fresh (id',ty)

let rec rename inj2cc = function
  | [] -> inj2cc,[]
  | ((x,i), (ToNothing | ToInternal _ as a)) :: t ->
    let inj,x' = rename_id inj2cc x in
    let inj',t' = rename inj t in
    inj',((x',i),a)::t'
  | ((x,i),ToNode (y,j)) :: t->
    let inj,x' = rename_id inj2cc x in
    let inj',y' = rename_id inj y in
    let inj'',t' = rename inj' t in
    inj'',((x',i),ToNode (y',j))::t'

let check_edge graph = function
  | ((Fresh (id,_),site),ToNothing) -> Edges.is_free id site graph
  | ((Fresh (id,_),site),ToInternal i) -> Edges.is_internal i id site graph
  | ((Fresh (id,_),site),ToNode (Existing id',site')) ->
    Edges.link_exists id site id' site' graph
  | ((Fresh (id,_),site),ToNode (Fresh (id',_),site')) ->
    Edges.link_exists id site id' site' graph
  | ((Existing id,site),ToNothing) -> Edges.is_free id site graph
  | ((Existing id,site),ToInternal i) -> Edges.is_internal i id site graph
  | ((Existing id,site),ToNode (Existing id',site')) ->
    Edges.link_exists id site id' site' graph
  | ((Existing id,site),ToNode (Fresh (id',_),site')) ->
    Edges.link_exists id site id' site' graph

(*inj is the partial injection built so far: inj:abs->concrete*)
let dst_is_okay inj' graph root site = function
  | ToNothing ->
    if Edges.is_free root site graph then Some inj' else None
  | ToInternal i ->
    if Edges.is_internal i root site graph then Some inj' else None
  | ToNode (Existing id',site') ->
    if Edges.link_exists root site
        (Renaming.apply inj' id') site' graph
    then Some inj' else None
  | ToNode (Fresh (id',ty),site') ->
    match Edges.exists_fresh root site ty site' graph with
    | None -> None
    | Some node -> Renaming.add id' node inj'

let injection_for_one_more_edge ?root inj graph = function
  | ((Existing id,site),dst) ->
    dst_is_okay inj graph (Renaming.apply inj id) site dst
  | ((Fresh (id,rty),site),dst) ->
    match root with
    | Some (root,rty') when rty=rty' ->
      (match Renaming.add id root inj with
       | None -> None
       | Some inj' -> dst_is_okay inj' graph root site dst)
    | _ -> None

let concretize_port inj = function
  | (Existing id,site) -> (Renaming.apply inj id,site)
  | (Fresh (id,_),site) -> (Renaming.apply inj id,site)

let concretize_arrow inj = function
  | ToNothing | ToInternal _ as x -> x
  | ToNode x -> ToNode (concretize_port inj x)

let concretize root graph nav =
  let out =
    List.fold_left
      (fun out (p,dst as step) ->
         match out with
         | None -> out
         | Some (root,acc,inj) ->
           match injection_for_one_more_edge ?root inj graph step with
           | None -> None
           | Some inj' ->
             let st = (concretize_port inj' p, concretize_arrow inj' dst) in
             Some (None,st::acc,inj'))
      (Some (Some root,[],Renaming.empty)) nav in
  Option_util.map (fun (_,l,_) -> List.rev l) out
