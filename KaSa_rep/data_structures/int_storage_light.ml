(**
   * int_storage.ml
   *
   * Creation:                      <2010-07-27 feret>
   * Last modification: Time-stamp: <Jun 17 2017>
   *
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   *
   * This library provides primitives to deal with storage functions
   *
   * Copyright 2010,2011,2012,2013,2014,2015 Institut National
   * de Recherche en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type ('a,'b) unary = 'a ->  'b
type ('a,'b,'c) binary = 'a -> 'b -> 'c
type ('a,'b,'c,'d) ternary = 'a -> 'b -> 'c -> 'd
type ('a,'b,'c,'d,'e) quaternary = 'a -> 'b -> 'c -> 'd -> 'e

type 'a unary_no_output = 'a -> unit
type ('a,'b) binary_no_output =  'a -> 'b -> unit
type ('a,'b,'c) ternary_no_output =  'a -> 'b -> 'c -> unit


module type Storage =
sig
  type 'a t
  type key
  type dimension

  val create: (dimension,'a t) unary
  val create_biggest_key: (key, 'a t) unary
  val expand_and_copy: ('a t,dimension,'a t) binary
  val init: (dimension, (key, 'a) unary, 'a t) binary
  val set: (key,'a,'a t,'a t) ternary
  val free: (key,'a t,'a t) binary
  val get: (key,'a t,'a option) binary
  val unsafe_get: (key,'a t,'a option) binary
  val dimension: ('a t, dimension) unary
  val print: (Format.formatter,'a unary_no_output,'a t) ternary_no_output
  val key_list: ('a t, key list) unary
  val iter:((key,'a) binary_no_output, 'a t) binary_no_output
  val fold_with_interruption: ((key,'a,'b,'b) ternary,'a t,'b,'b) ternary
  val fold: ((key,'a,'b,'b) ternary,'a t,'b,'b) ternary
  val fold2_common: ((key,'a,'b,'c,'c) quaternary,'a t,'b t, 'c, 'c) quaternary
  val free_all: ('a t,'a t) unary
end

let invalid_arg pos exn value  = assert false

module Int_storage_imperatif =
  (struct
    type key = int
    type dimension = int
    type 'a t =
      {
        array:('a option) array ;
        size:int ;
      }

    let dimension a = a.size

    let key_list t =
      let size = t.size in
      let array = t.array in
      let rec aux k sol =
        if k<0 then sol
        else
          match array.(k) with
          | None -> aux (k-1) sol
          | Some _ -> aux (k-1) (k::sol)
      in aux size []

    let rec create size  =
      if size < 0
      then
        let array = create 0 in
        invalid_arg __POS__ Exit array
      else

        {
          array = Array.make (size+1) None;
          size = size;
        }

    let create_biggest_key x = create x

    let expand_and_copy array size =
      let dimension = dimension array in
      if dimension < size
      then
        let array' = create size in
        let _ = Array.blit array.array 0 array'.array 0 dimension in
        array'
      else
        {array = Array.sub array.array 0 size ; size = size}

    let set key value array =
      if key>array.size || key<0
      then
        let () = Printf.fprintf stdout "%i %i" key array.size in
        invalid_arg __POS__ Exit array
      else
        let _ = array.array.(key)<-Some value in
        array

    let rec init size f =
      if size < 0
      then
        let array = create 0 in
        invalid_arg __POS__ Exit array
      else
        let array = create size in
        let rec aux k array =
          if k>size then array
          else
            let value = f k in
            let array = set k value array in
            aux (k+1) array
        in
        aux 0 array

    let get key array =
      if key>array.size || key<0 then
        invalid_arg __POS__ Exit None
      else
        match array.array.(key) with
        | None -> invalid_arg __POS__ Exit None
        | a -> a

    let free key array =
      if key>array.size || key<0 then
        let _ = invalid_arg __POS__ Exit None in
        array
      else
      match array.array.(key) with
      | None ->
        let _ = invalid_arg __POS__ Exit None in
        array
      | _ ->
        let () = array.array.(key)<-None in
        array


    let unsafe_get key array =
      if key>array.size || key<0 then
        None
      else
        array.array.(key)

    let print fmt print_elt array =
      let rec aux i =
        if i>array.size then ()
        else
          let () =
            match array.array.(i) with
            | None -> ()
            | Some elt ->
              let () =
                Format.fprintf fmt
                  "%d: @."  i in
              let () = print_elt elt in
              ()
          in aux (i+1)
      in aux 0

    let iter f t =
      let size = t.size in
      let array = t.array in
      let rec aux k =
        if k>size then ()
        else
          match array.(k) with
          | None ->
            aux (k+1)
          | Some x ->
            let () = f k x in
            aux (k+1)
      in
      aux 0

    let fold f t init =
      let size = t.size in
      let array = t.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array.(k) with
          | None ->
            aux (k+1) remanent
          | Some x ->
            let sol  = remanent in
            aux (k+1) (f k x  sol)
      in
      aux 0 init

    let fold_with_interruption f t init =
      let size = t.size in
      let array = t.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array.(k) with
          | None ->
            aux (k+1) remanent
          | Some x ->
            let sol  = remanent in
            let output_opt =
              try
                Some (f k x sol)
              with
                Sys.Break -> None
            in
            match
              output_opt
            with
            | None -> remanent
            | Some a -> aux (k+1) a
      in
      aux 0 init

    let fold2_common f t1 t2 init =
      let size = min t1.size t2.size in
      let array1 = t1.array in
      let array2 = t2.array in
      let rec aux k remanent =
        if k>size then remanent
        else
          match array1.(k),array2.(k) with
          | None,_ | _,None -> aux (k+1) remanent
          | Some x1,Some x2 ->
            let sol = remanent in
            aux (k+1) (f k x1 x2 sol)
      in
      aux 0  (init)

    let free_all t =
      fold
        (fun a _ t -> free a t)
        t t


  end:Storage with type key = int and type dimension = int)

module Nearly_infinite_arrays =
  functor (Basic:Storage with type dimension = int and type key = int) ->
    (struct
      type dimension = Basic.dimension
      type key = Basic.key
      type 'a t = 'a Basic.t

      let create = Basic.create
      let create_biggest_key = Basic.create_biggest_key
      let dimension = Basic.dimension
      let key_list = Basic.key_list

      let expand array =
        let old_dimension = dimension array in
        if old_dimension = Sys.max_array_length
        then
          invalid_arg __POS__ Exit array
        else
          Basic.expand_and_copy array
            (max 1 (min Sys.max_array_length (2*old_dimension)))

      let get = Basic.get
      let unsafe_get = Basic.unsafe_get
      let expand_and_copy = Basic.expand_and_copy
      let init = Basic.init
      let free = Basic.free
      let rec set key value array =
        let dimension = dimension array in
        if key>=dimension
        then
          let array' = expand array in
          if array == array'
          then
            invalid_arg __POS__ Exit array
          else
            set key value array'
        else
          Basic.set key value array

      let print = Basic.print
      (*      let print_var_f = Basic.print_var_f
              let print_site_f = Basic.print_site_f*)
      let iter = Basic.iter
      let fold = Basic.fold
      let fold_with_interruption = Basic.fold_with_interruption
      let fold2_common = Basic.fold2_common
      let free_all = Basic.free_all
    end:Storage with type key = int and type dimension = int)

module Extend =
  functor (Extension:Storage) ->
  functor (Underlying:Storage) ->
    (struct

      type dimension = Extension.dimension * Underlying.dimension

      type key = Extension.key * Underlying.key

      type 'a t =
        {
          matrix : 'a Underlying.t Extension.t ;
          dimension : dimension;
        }

      let create dimension =
        let matrix = Extension.create (fst dimension) in

        {
          matrix = matrix;
          dimension = dimension ;
        }

      let create_biggest_key key =
        let matrix = Extension.create_biggest_key (fst key) in
        let matrix' = Underlying.create_biggest_key (snd key) in
        let dimension = Extension.dimension matrix in
        let dimension' = Underlying.dimension matrix' in

        {matrix = matrix;
         dimension = (dimension,dimension')}

      let key_list t =
        let ext_list = Extension.key_list t.matrix in
        List.fold_left
          (fun (list) key ->
             let t2 = Extension.get key t.matrix in
             match t2 with
             | None -> invalid_arg __POS__ Exit list
             | Some t2 ->
               let l2 = Underlying.key_list t2 in

               List.fold_left
                 (fun list key2  -> (key,key2)::list)
                 list
                 (List.rev l2))
          ([])
          (List.rev ext_list)

      let expand_and_copy array _dimension =
        invalid_arg __POS__ Exit array

      let init dim  f =
        let array =
          Extension.init (fst dim)
            (fun i ->
               Underlying.init (snd dim) (fun j -> f (i,j)))
        in

        {
          matrix = array;
          dimension = dim
        }

      let set (i,j) value array =
        let old_underlying = Extension.unsafe_get i array.matrix in
        let old_underlying =
          match old_underlying with
          | Some old_underlying -> old_underlying
          | None -> Underlying.create (snd array.dimension)
        in
        let new_underlying = Underlying.set j value old_underlying in
        let new_matrix = Extension.set i new_underlying array.matrix in
        (* let ordered = ordered && Extension.ordered new_matrix in*)
        {array with matrix = new_matrix}

      let get (i,j) array =
        let underlying = Extension.get i array.matrix in
        match underlying with
        | Some underlying -> Underlying.get j underlying
        | None ->  invalid_arg __POS__ Exit None

      let unsafe_get (i,j) array =
        let underlying = Extension.unsafe_get i array.matrix in
        match underlying with
        | Some underlying -> Underlying.unsafe_get j underlying
        | _ -> None

      let free (i,j) array =
        let old_underlying = Extension.unsafe_get i array.matrix in

          match old_underlying with
          | None ->
            let _ = invalid_arg __POS__ Exit None in
             array
          | Some old_underlying ->
            let new_underlying = Underlying.free j  old_underlying in
            let new_matrix = Extension.set i
                new_underlying array.matrix in
            {array with matrix = new_matrix}

      let dimension a = a.dimension


      let print fmt print_of a =
        Extension.print fmt
          (fun a -> Underlying.print fmt print_of a)
          a.matrix

      let iter f a =
        Extension.iter
          (fun k a ->
             Underlying.iter
               (fun k' a' -> f (k,k') a')
               a
          )
          a.matrix

      let fold_gen fold1 fold2 f a b =
        fold1
          (fun k a b ->
             fold2
               (fun k' a' b -> f (k,k') a' b)
               a
               b
          )
          a.matrix
          b

      let fold f a b = fold_gen Extension.fold Underlying.fold f a b
      let fold_with_interruption f a b =
        fold_gen Extension.fold_with_interruption
          Underlying.fold_with_interruption f a b

      let fold2_common f a b c =
        fold
          (fun k a c ->
             let get = unsafe_get k b in
             match get with
             | None -> c
             | Some b -> f k a b c)
          a
          c

      let free_all t =
        fold
          (fun a _ t -> free a t)
          t t

    end:Storage with type key = Extension.key * Underlying.key and type dimension = Extension.dimension * Underlying.dimension )


module Quick_key_list =
  functor (Basic:Storage) ->
    (struct
      type dimension = Basic.dimension
      type key = Basic.key
      type 'a t =
        {
          basic: 'a Basic.t ;
          keys: key list
        }

      let create i =
        let basic = Basic.create i in
        {basic = basic ; keys = []}

      let create_biggest_key key =
        let basic = Basic.create_biggest_key key in
        {basic = basic ; keys = []}

      let key_list t = t.keys

      let expand_and_copy array j =
        let basic = Basic.expand_and_copy array.basic j in
        {basic = basic ; keys = array.keys}

      let init n f =
        let basic = Basic.init n f in
        let keys =
          Basic.fold
            (fun k _ list -> k::list)
            basic []
        in
        {basic = basic; keys = keys}

      let set key value array =
        let old = Basic.unsafe_get key array.basic in
        let new_array =
          match old with
          | Some _ -> array
          | None -> {array with keys = key::array.keys}
        in
        let new_basic = Basic.set key value new_array.basic in
        {new_array with basic = new_basic}

      let free key array =
        let basic = Basic.free key array.basic in
        {array with basic = basic}


      let get key array =
        Basic.get key array.basic

      let unsafe_get key array =
        Basic.unsafe_get key array.basic

      let dimension a = Basic.dimension a.basic

      let print fmt f a = Basic.print fmt f a.basic

      let iter f a =
        let list = key_list a in
        List.iter
          (fun k ->
             let im = get k a in
             match im with
             | None ->
               let _ =
                 invalid_arg
                   __POS__
                   Exit ()
               in ()
             | Some im -> f k im)
          (List.rev list)

      let fold f a b =
        let list = key_list a in
        List.fold_left
          (fun (b) k ->
             let im = get k a in
             match im with
             | None -> invalid_arg __POS__ Exit b
             | Some im -> f k im b)
          (b)
          (List.rev list)

      let free_all t =
        let t =
          fold
              (fun a _ t -> free a t)
              t t
        in
        {t with keys = []}

      let fold_with_interruption f a b =
        let list = key_list a in
        let rec aux list output =
          match
            list
          with
          | [] -> output
          | head::tail ->
            let output_opt =
              try
                let im = get head a in
                match im with
                | None ->
                  Some (invalid_arg __POS__ Exit output)
                | Some im -> Some (f head im output)
              with Sys.Break -> None
            in
            match
              output_opt
            with
            | None -> output
            | Some output -> aux tail output
        in aux list (b)

      let fold2_common f a b c =
        fold
          (fun k a c ->
             let get = unsafe_get k b in
             match get with
             | None -> c
             | Some b -> f k a b c)
          a
          c

    end:Storage with type key = Basic.key and type dimension = Basic.dimension)

module Nearly_inf_Imperatif = Nearly_infinite_arrays (Int_storage_imperatif)

module Quick_Nearly_inf_Imperatif = Quick_key_list (Nearly_inf_Imperatif)

module Int_Int_storage_Imperatif_Imperatif =
  Extend (Int_storage_imperatif)(Int_storage_imperatif)

module Nearly_Inf_Int_Int_storage_Imperatif_Imperatif =
  Extend (Quick_Nearly_inf_Imperatif)(Quick_Nearly_inf_Imperatif)

module Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif =
  Extend (Quick_Nearly_inf_Imperatif)
    (Extend (Quick_Nearly_inf_Imperatif)(Quick_Nearly_inf_Imperatif))
