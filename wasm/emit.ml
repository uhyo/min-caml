open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

(* super-tenuki global variables *)
let fentries = ref []

let rec findi f = findi' f 0
and findi' f i = function
  | [] -> None
  | x::xs ->
      if f x then Some i
      else findi' f (i+1) xs

(* WAT style of function name. *)
let func_name (Id.L(x)) = "$" ^ x
let local_name x = "$" ^ x

let wat_type = function
  | Type.Float -> "f32"
  | _ -> "i32"

let func_sig oc = function
  | Type.Fun(tys, ret) ->
      List.iter
        (fun ty ->
           if ty <> Type.Unit then
             Printf.fprintf oc "(param %s)" (wat_type ty))
        tys;
      if ret <> Type.Unit then
        Printf.fprintf oc " (result %s)" (wat_type ret);
  | _ -> assert false 

let rec g oc = function
  | Ans(e) -> gexp oc e
  | Let((x, t), e, n) ->
      (* Calculation of `e` should leave a variable when t is not unit. *)
      gexp oc e;
      if t <> Type.Unit then
        begin
          Printf.fprintf oc
            "    set_local %s\n"
            (local_name x)
        end;
      g oc n
and gexp oc = function
  | Nop -> ()
  | Consti(i) ->
      Printf.fprintf oc "    i32.const %d\n" i
  | Constf(f) ->
      Printf.fprintf oc "    f32.const %f\n" f
  | Add(x, y) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.add\n";
  | Sub(x, y) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.sub\n";
  | Mul(x, y) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.mul\n";
  | Div(x, y) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.div_s\n";
  | Shl(x, y) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.shl\n";
  | FAdd(x, y) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.add\n";
  | FSub(x, y) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.sub\n";
  | FMul(x, y) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.mul\n";
  | FDiv(x, y) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.div_s\n";
  | Loadi(x, c) ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      Printf.fprintf oc "    i32.load offset=%d align=4\n" c;
  | Loadf(x, c) ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      Printf.fprintf oc "    f32.load offset=%d align=4\n" c;
  | Storei(v, x, c) ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      gv oc v;
      Printf.fprintf oc "    i32.store offset=%d align=4\n" c;
  | Storef(v, x, c) ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      gvf oc v;
      Printf.fprintf oc "    f32.store offset=%d align=4\n" c;
  | GetGlobal(x) ->
      Printf.fprintf oc "    get_global %s\n" (local_name x);
  | SetGlobal(v, x) ->
      gv oc v;
      Printf.fprintf oc "    set_global %s\n" (local_name x);
  | IfEq(ty, x, y, n1, n2) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.eq\n";
      if ty = Type.Unit then
        Printf.fprintf oc "    (if (then\n"
      else
        Printf.fprintf oc "    (if (result %s) (then\n" (wat_type ty);
      g oc n1;
      Printf.fprintf oc "    )\n    (else\n";
      g oc n2;
      Printf.fprintf oc "    ))\n";
  | IfLE(ty, x, y, n1, n2) ->
      gv oc x;
      gv oc y;
      Printf.fprintf oc "    i32.le_s\n";
      if ty = Type.Unit then
        Printf.fprintf oc "    (if (then\n"
      else
        Printf.fprintf oc "    (if (result %s) (then\n" (wat_type ty);
      g oc n1;
      Printf.fprintf oc "    )\n    (else\n";
      g oc n2;
      Printf.fprintf oc "    ))\n";
  | IfFEq(ty, x, y, n1, n2) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.eq\n";
      if ty = Type.Unit then
        Printf.fprintf oc "    (if (then\n"
      else
        Printf.fprintf oc "    (if (result %s) (then\n" (wat_type ty);
      g oc n1;
      Printf.fprintf oc "    )\n    (else\n";
      g oc n2;
      Printf.fprintf oc "    ))\n";
  | IfFLE(ty, x, y, n1, n2) ->
      gvf oc x;
      gvf oc y;
      Printf.fprintf oc "    f32.le\n";
      if ty = Type.Unit then
        Printf.fprintf oc "    (if (then\n"
      else
        Printf.fprintf oc "    (if (result %s) (then\n" (wat_type ty);
      g oc n1;
      Printf.fprintf oc "    )\n    (else\n";
      g oc n2;
      Printf.fprintf oc "    ))\n";
  | CallCls(f, sign, args, fargs) ->
      (* push arguments onto the stack. *)
      List.iter (gv oc) args;
      List.iter (gvf oc) fargs;
      (* Load function index. *)
      Printf.fprintf oc "    get_local %s\n" (local_name f);
      Printf.fprintf oc "    i32.load offset=0 align=4\n";
      (* call. *)
      Printf.fprintf oc "    call_indirect %s\n" (local_name sign);
  | CallDir(f, args, fargs) ->
      (* push arguments onto the stack. *)
      List.iter (gv oc) args;
      List.iter (gvf oc) fargs;
      (* call. *)
      Printf.fprintf oc "    call %s\n" (func_name f);
  | Var(x) ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
  | FunTableIndex(x) ->
      begin
        match findi (fun e -> e.name = x) !fentries with
          | Some(i) ->
              Printf.fprintf oc "    i32.const %d\n" i;
          | None ->
              failwith "Unknown function name "
      end
  | ExtArray(x) ->
      Printf.fprintf oc "    TODO ;; ExtArray\n"
and gv oc = function
  | V(x) -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | C(i) -> Printf.fprintf oc "    i32.const %d\n" i
and gvf oc = function
  | Vf(x) -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | F(f) -> Printf.fprintf oc "    f32.const %f\n" f

let h oc { name; args; fargs; body = e; ret } =
  (* Emit a function definition. *)
  Printf.fprintf oc "  (func %s" (func_name name);
  (* Declare function signature. *)
  List.iter
    (fun x ->
       Printf.fprintf oc " (param %s %s) "
         (local_name x)
         (wat_type Type.Int))
    args;
  List.iter
    (fun x ->
       Printf.fprintf oc " (param %s %s) "
         (local_name x)
         (wat_type Type.Float))
    fargs;
  if ret <> Type.Unit then
    Printf.fprintf oc " (result %s)"
      (wat_type ret);
  (* Declare local variables. *)
  Printf.fprintf oc "\n   ";
  List.iter
    (fun (x, t) ->
       Printf.fprintf oc " (local %s %s)"
         (local_name x)
         (wat_type t))
    (localvs e);
  (* Body of function. *)
  Printf.fprintf oc "\n";
  g oc e;
  Printf.fprintf oc ")\n"

let f oc {typesigs; funtable; fundefs; externals; start} =
  fentries := funtable;
  Format.eprintf "generating assembly...@.";
  (* Currently we use the Text Format of WebAssembly. *)
  Printf.fprintf oc "(module\n";
  (* Emit declared type signatures. *)
  List.iter
    (fun (s, t) ->
       Printf.fprintf oc "  (type %s (func "
         (local_name s);
       func_sig oc t;
       Printf.fprintf oc "))\n")
    typesigs;
  (* Emit import declarations. *)
  let flg_create_array = ref false in
  let flg_create_float_array = ref false in
  List.iter
    (fun (Id.L(x), t) ->
       (* ad-hoc interruption of importing *)
       match x with
         | "min_caml_create_array" ->
             flg_create_array := true
         | "min_caml_create_float_array" ->
             flg_create_float_array := true
         | _ ->
             begin
               Printf.fprintf oc "  (import \"%s\" \"%s\" (func %s "
                 "lib"
                 x
                 (func_name (Id.L(x)));
               func_sig oc t;
               Printf.fprintf oc "))\n";
             end)
    externals;

  (* Emit function definitions. *)
  List.iter (h oc) fundefs;
  (* Generate the function index table. *)
  if funtable <> [] then
    begin
      Printf.fprintf oc "  (table %d anyfunc)\n" (List.length funtable);
      Printf.fprintf oc "  (elem (i32.const 0)";
      List.iter
        (fun {name} -> Printf.fprintf oc " %s" (func_name name))
        funtable;
      Printf.fprintf oc ")\n";
    end;
  (* Additional functions to communicate with external library. *)
  if !flg_create_array then begin
    Printf.fprintf oc "%s\n"
      (String.concat "\n" [
        "  (func $min_caml_create_array (param $num i32) (param $value i32) (result i32)";
        "    (local $i i32) (local $res i32)";
        Printf.sprintf "    get_global %s" (local_name global_hp);
        "    set_local $res";
        "    i32.const 0";
        "    set_local $i";
        "    get_local $num";
        "    i32.const 2";
        "    i32.shl";
        "    set_local $num";
        "    loop $loop";
        "      get_local $i";
        "      get_local $num";
        "      i32.lt_u";
        "      if";
        Printf.sprintf "        get_global %s" (local_name global_hp);
        "        get_local $i";
        "        i32.add";
        "        get_local $value";
        "        i32.store offset=0 align=4";
        "        get_local $i";
        "        i32.const 4";
        "        i32.add";
        "        set_local $i";
        "        br $loop";
        "      end";
        "    end";
        Printf.sprintf "    get_global %s" (local_name global_hp);
        "    get_local $num";
        "    i32.add";
        Printf.sprintf "    set_global %s" (local_name global_hp);
        "    get_local $res)";
      ])
  end;
  if !flg_create_float_array then begin
    Printf.fprintf oc "%s\n"
      (String.concat "\n" [
        "  (func $min_caml_create_float_array (param $num i32) (param $value f32) (result i32)";
        "    (local $i i32) (local $res i32)";
        Printf.sprintf "    get_global %s" (local_name global_hp);
        "    set_local $res";
        "    i32.const 0";
        "    set_local $i";
        "    get_local $num";
        "    i32.const 2";
        "    i32.shl";
        "    set_local $num";
        "    loop $loop";
        "      get_local $i";
        "      get_local $num";
        "      i32.lt_u";
        "      if";
        Printf.sprintf "        get_global %s" (local_name global_hp);
        "        get_local $i";
        "        i32.add";
        "        get_local $value";
        "        f32.store offset=0 align=4";
        "        get_local $i";
        "        i32.const 4";
        "        i32.add";
        "        set_local $i";
        "        br $loop";
        "      end";
        "    end";
        Printf.sprintf "    get_global %s" (local_name global_hp);
        "    get_local $num";
        "    i32.add";
        Printf.sprintf "    set_global %s" (local_name global_hp);
        "    get_local $res)";
      ])
  end;
  (* Require 192KiB of memory. *)
  Printf.fprintf oc "  (memory %d)\n" 3;
  (* Declare global variables. *)
  Printf.fprintf oc "  (global %s (mut i32) i32.const 0)\n"
    (local_name global_hp);
  Printf.fprintf oc "  (global %s (mut i32) i32.const 0)\n"
    (local_name global_cp);
  (* Declare start function. *)
  Printf.fprintf oc "  (start %s)\n" (func_name start);
  (* End of module. *)
  Printf.fprintf oc ")\n";
