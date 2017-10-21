val extension: string

type id_or_imm = V of Id.t | C of int
type id_or_immf= Vf of Id.t | F of float
type t = 
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = 
  | Nop
  (* const *)
  | Consti of int
  | Constf of float
  (* arithmetic *)
  | Add of id_or_imm * id_or_imm
  | Sub of id_or_imm * id_or_imm
  | Mul of id_or_imm * id_or_imm
  | Div of id_or_imm * id_or_imm
  | Shl of id_or_imm * id_or_imm
  | FAdd of id_or_immf * id_or_immf
  | FSub of id_or_immf * id_or_immf
  | FMul of id_or_immf * id_or_immf
  | FDiv of id_or_immf * id_or_immf
  (* linear memory *)
  | Loadi of Id.t * int
  | Loadf of Id.t * int
  | Storei of id_or_imm * Id.t * int (* value, address, offset *)
  | Storef of id_or_immf * Id.t * int
  (* globals *)
  | GetGlobal of Id.t
  | SetGlobal of id_or_imm * Id.t (* value, global name *)
  (* control instructions *)
  | IfEq of Type.t * id_or_imm * id_or_imm * t * t
  | IfLE of Type.t * id_or_imm * id_or_imm * t * t
  | IfFEq of Type.t * id_or_immf * id_or_immf * t * t
  | IfFLE of Type.t * id_or_immf * id_or_immf * t * t
  (* closure address, expected closure type, arguments *)
  | CallCls of Id.t * Id.t * id_or_imm list * id_or_immf list
  | CallDir of Id.l * id_or_imm list * id_or_immf list
  (* virtual instructions *)
  | Var of Id.t
  | FunTableIndex of Id.l (* get index of function registerd in *the* table. *)
  | ExtArray of Id.l

(* function definition. *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* function table entry. *)
type fentry = {
  name: Id.l;
  ty: Type.t;
}
 
(* Module (whole program)
 * A module consists of:
 * * a set of type signatures.
 * * a function table.
 * * a set of function definitions.
 * * a set of imported functions.
 * * name of start function.
 *)
type prog = {
  typesigs: (Id.t * Type.t) list;
  funtable: fentry list;
  fundefs: fundef list;
  externals: (Id.l * Type.t) list;
  start: Id.l;
}

(* Global variables. *)
val global_hp : Id.t
val global_cp : Id.t

val seq : exp * t -> t

val fv_exp : exp -> Id.t list
val fv : t -> Id.t list
val localvs: t -> (Id.t * Type.t) list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int
