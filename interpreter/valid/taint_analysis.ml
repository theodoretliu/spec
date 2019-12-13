open Ast
open Source
open Types
open Valid

type taint = Taint | NotTaint
type taint_func_type = (taint list * taint list)
type taint_context = {
  functions: taint_func_type list;
  stack: taint list;
  locals: taint list;
  globals: taint list;
}

let empty_context = {
  functions = [];
  stack = [];
  locals = [];
  globals = [];
}

module Invalid = Error.Make ()
exception Invalid = Invalid.Error

let error = Invalid.error
let require b at s = if not b then error at s

let lookup category list x =
  try Lib.List32.nth list x.it with Failure _ ->
    error x.at ("unknown " ^ category ^ " " ^ Int32.to_string x.it)

let type_ (c : taint_context) x = lookup "type" c.types x
let func (c : taint_context) x = lookup "function" c.funcs x
let table (c : taint_context) x = lookup "table" c.tables x
let memory (c : taint_context) x = lookup "memory" c.memories x
let global (c : taint_context) x = lookup "global" c.globals x
let local (c : taint_context) x = lookup "local" c.locals x
let label (c : taint_context) x = lookup "label" c.labels x

(* let check_taint_function (f: func) (c: taint_context) : taint_context =
  match f.it with
  | { ftype; locals; body } -> *)

let func_hardcode = [
  "print", ([NotTaint], [])
]

let global_hardcode = [
  "global_taint", Taint
]

let rec repeat n v =
  if n = 0 then []
  else v :: repeat (n - 1) v

let func_to_taint (FuncType (ins, outs)) : taint_func_type =
  let new_ins = repeat (List.length ins) NotTaint in
  let new_outs = repeat (List.length outs) NotTaint in
  new_ins, new_outs

let add_import (ts : func_type list) (im : import) (c : taint_context) : taint_context =
  let {module_name ; item_name ; idesc} = im.it in
  match idesc.it with
  | FuncImport x ->
      begin try
        let func_type = List.assoc (Ast.string_of_name item_name) func_hardcode in
        { c with functions = func_type :: c.functions }
      with Not_found ->
        let new_f = func_to_taint (lookup "type" ts x) in
        { c with functions = new_f :: c.functions }
      end
  | TableImport tt -> failwith "todo: table import"
  | MemoryImport mt -> failwith "todo memory import"
  | GlobalImport gt ->
    begin try
      let global_type = List.assoc (Ast.string_of_name item_name) global_hardcode in
      { c with globals = global_type :: c.globals }
    with Not_found ->
      { c with globals = NotTaint :: c.globals }
    end

let  check_taint_function (f : func) (c : taint_context) : taint_context =
  let {ftype; locals; body} = f.it in
  let new_c = { c with locals = repeat (List.length locals) NotTaint } in





let check_taint_module (m : module_) =
  let
    { types; imports; tables; memories; globals; funcs; start; elems; data;
      exports } = m.it
  in
  let types = List.map (fun ty -> ty.it) types in
  let c0 = List.fold_right (add_import types) imports empty_context in
  let c =
    { c0 with
      functions = c0.functions @ List.map (fun f -> type_ c0 f.it.ftype |> func_to_taint) funcs;
      (* tables = c0.tables @ List.map (fun tab -> tab.it.ttype) tables;
      memories = c0.memories @ List.map (fun mem -> mem.it.mtype) memories; *)
      globals = c0.globals @ (repeat (List.length globals) NotTaint)
    }
  in
  (* List.iter check_type types;
  List.iter (check_global c1) globals;
  List.iter (check_table c1) tables;
  List.iter (check_memory c1) memories;
  List.iter (check_elem c1) elems;
  List.iter (check_data c1) data;
  List.iter (check_func c) funcs;
  check_start c start;
  ignore (List.fold_left (check_export c) NameSet.empty exports);
  require (List.length c.tables <= 1) m.at
    "multiple tables are not allowed (yet)";
  require (List.length c.memories <= 1) m.at
    "multiple memories are not allowed (yet)" *)

  (* let initial_context = {
    stack = [];
    locals = [];
    globals = [
      "tainted_global", Taint
    ];

    functions = [
      "print", ([NotTaint], [])
    ];
  } in *)

  let fold acc f = check_taint_function f {acc with stack = []; locals = []} in
  let c = List.fold_left fold c funcs in
  let c = List.fold_left fold c funcs in (* second pass *)
  ()
