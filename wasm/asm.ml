(* WebAssembly (virtual) instructions *)

(* extension of assembly file. *)
let extension = ".wat"

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
  | CallCls of Id.t * Id.t * Id.t list
  | CallDir of Id.l * Id.t list
  (* virtual instructions *)
  | Var of Id.t
  | FunTableIndex of Id.l (* get index of function registerd in *the* table. *)
  | ExtArray of Id.l
type fundef = { name : Id.l; args : (Id.t * Type.t) list; body : t; ret : Type.t }
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

(* Global variables *)
let global_hp = "@hp" (* heap pointer *)
let global_cp = "@cp" (* closure pointer *)


let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_imm = function
  | V(x) -> [x]
  | _ -> []
let fv_immf = function
  | Vf(x) -> [x]
  | _ -> []
let rec fv_exp = function
  | Nop | Consti(_) | Constf(_) | GetGlobal(_) 
  | FunTableIndex(_) | ExtArray(_) -> []
  | Loadi(x, _)  | Loadf(x, _)
  | Var(x) -> [x]
  | SetGlobal(x, _) -> fv_imm x
  | Storei(x, y, _) -> y :: fv_imm x
  | Storef(x, y, _) -> y :: fv_immf x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y)
  | Shl(x, y) -> fv_imm x @fv_imm y
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) -> fv_immf x @ fv_immf y
  | IfEq(_, x, y, e1, e2) | IfLE(_, x, y, e1, e2) -> fv_imm x @ fv_imm y @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | IfFEq(_, x, y, e1, e2) | IfFLE(_, x, y, e1, e2) -> fv_immf x @ fv_immf y @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | CallCls(x, _, ys) -> x :: ys
  | CallDir(_, ys) -> ys
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

(* all local variables *)
let rec localvs_exp = function
  | IfEq(_, _, _, e1, e2) | IfLE(_, _, _, e1, e2)
  | IfFEq(_, _, _, e1, e2) | IfFLE(_, _, _, e1, e2) ->
      localvs e1 @ localvs e2
  | _ -> []
and localvs = function
  | Ans(exp) -> localvs_exp exp
  | Let(xt, exp, e) -> xt :: localvs_exp exp @ localvs e

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)
