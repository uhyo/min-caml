val extension: string

type t = 
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = 
  | Nop
  (* const *)
  | Consti of int
  | Constf of float
  (* arithmetic *)
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | Shl of Id.t * Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  (* linear memory *)
  | Loadi of Id.t * int
  | Loadf of Id.t * int
  | Storei of Id.t * Id.t * int (* value, address, offset *)
  | Storef of Id.t * Id.t * int
  (* globals *)
  | GetGlobal of Id.t
  | SetGlobal of Id.t * Id.t (* value, global name *)
  (* control instructions *)
  | IfEq of Type.t * Id.t * Id.t * t * t
  | IfLE of Type.t * Id.t * Id.t * t * t
  | IfFEq of Type.t * Id.t * Id.t * t * t
  | IfFLE of Type.t * Id.t * Id.t * t * t
  (* closure address, arguments *)
  | CallCls of Id.t * Id.t list
  | CallDir of Id.l * Id.t list
  (* virtual instructions *)
  | Var of Id.t
  | FunTableIndex of Id.l (* get index of function registerd in *the* table. *)
  | ExtArray of Id.l (* TODO *)

(* function definition. *)
type fundef = { name : Id.l; args : (Id.t * Type.t) list; body : t; ret : Type.t }
(* function table entry. *)
type fentry = {
  name: Id.l;
  ty: Type.t;
}
 
(* Module (whole program)
 * A module consists of:
 * * a function table.
 * * a set of function definitions.
 * * name of start function.
 *)
type prog = {
  funtable: fentry list;
  fundefs: fundef list;
  externals: (Id.l * Type.t) list;
  start: Id.l;
}

(* Global variables. *)
val global_hp : Id.t

val seq : exp * t -> t

val fv : t -> Id.t list
val localvs: t -> (Id.t * Type.t) list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int
