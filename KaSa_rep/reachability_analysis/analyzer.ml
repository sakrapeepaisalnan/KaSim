(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Analyzer =
  sig

    type static_information
    type dynamic_information

    val main:
      Remanent_parameters_sig.parameters ->
      Exception.method_handler ->
      Mvbdu_wrapper.Mvbdu.handler ->
      Cckappa_sig.compil ->
      Cckappa_sig.kappa_handler ->
      Exception.method_handler * static_information * dynamic_information

    val export:
      static_information ->
      dynamic_information ->
      Exception.method_handler ->
      Analyzer_headers.kasa_state ->
      Exception.method_handler * dynamic_information * Analyzer_headers.kasa_state

    val print:
      static_information ->
      dynamic_information ->
      Exception.method_handler ->
      Loggers.t list ->
      Exception.method_handler * dynamic_information

  end

(*****************************************************************************************)
(*Analyzer is a functor takes a module Domain as its parameter.*)

module Make (Domain:Composite_domain.Composite_domain) =
struct

  type static_information =
    Domain.static_information

  type dynamic_information =
    Domain.dynamic_information

  let main parameter error mvbdu_handler compil kappa_handler =
    let error, static, dynamic = 
      Analyzer_headers.initialize_global_information
        parameter error mvbdu_handler compil kappa_handler 
    in
    let error, init = Analyzer_headers.compute_initial_state error static in
    let error, static, dynamic = Domain.initialize static dynamic error in
    let error, dynamic =
      List.fold_left
	(fun (error, dynamic) chemical_species ->
	  let error, dynamic, () =
            Domain.add_initial_state static dynamic error chemical_species 
          in
	  error, dynamic)
	(error, dynamic)
	init
    in
    let rec aux error dynamic =
      let error, dynamic, next_opt = Domain.next_rule static dynamic error in
      match next_opt with
      | None -> error, static, dynamic
      | Some r_id ->
	begin
	  let error, dynamic, is_enabled =
            Domain.is_enabled static dynamic error r_id 
          in
	  match is_enabled with
	  | None -> aux error dynamic
	  | Some precondition ->
	    let error, dynamic, () =
              Domain.apply_rule static dynamic error r_id precondition 
            in
	    aux error dynamic
	end
    in aux error dynamic
    
  let export static dynamic error kasa_state =
    Domain.export static dynamic error kasa_state
      
  let print static dynamic error loggers =
    let error, dynamic, () = Domain.print static dynamic error loggers in
    error, dynamic
end
