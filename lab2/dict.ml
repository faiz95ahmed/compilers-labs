(* lab2/dict.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

(* Environments are implemented using a library module that 
   represents mappings by balanced binary trees. *)

type ident = string

type ptype = 
    Integer 
  | Boolean 
  | Array of int * ptype
  | Void

(* |def| -- definitions in environment *)
type def = 
  { d_tag: ident;               (* Name *)
    d_type: ptype;              (* Type *)
    d_lab: string }             (* Global label *)

module IdMap = Map.Make(struct type t = ident  let compare = compare end)

type environment = Env of def IdMap.t

(* |err_line| -- line number for error messages *)
let err_line = ref 1

(* |Dictionary_error| -- exception raised if error detected *)
exception Dictionary_error of string * int

(* |dict_error| -- issue error message by raising exception *)
let dict_error fmt = 
  raise (Dictionary_error (fmt, !err_line))

let can f x = try f x; true with Not_found -> false

(* |define| -- add a definition *)
let define d (Env e) = 
  if can (IdMap.find d.d_tag) e then raise Exit;
  Env (IdMap.add d.d_tag d e)

(* |lookup| -- find definition of an identifier *)
let lookup x (Env e) = IdMap.find x e

(* |init_env| -- empty environment *)
let init_env = Env IdMap.empty

(* |type_size| -- the size occupied by a value of a given type *)
let rec type_size x = 
  match x with
      Integer -> 4
    | Boolean -> 1
    | Array (size, basetype) -> size * (type_size basetype)
    | _ -> dict_error "void type"

(* |is_array| -- returns false for the types Integer and Boolean, and true for array types *)
let is_array x = 
  match x with
      Array (_,_) -> true
    | _ -> false

(* |base_type| -- the underlying type for elements of the array *)
let base_type x = 
  match x with
      Array (_, basetype) -> basetype
    | _ -> dict_error "not an array type"