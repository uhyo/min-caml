(* WebAssembly (virtual) instructions *)

(* extension of assembly file. *)
let extension = ".wat"

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
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list
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
 * * a function table.
 * * a set of function definitions.
 * * a set of imported functions.
 * * name of start function.
 *)
type prog = {
  funtable: fentry list;
  fundefs: fundef list;
  externals: (Id.l * Type.t) list;
  start: Id.l;
}

(* Global variables *)
let global_hp = "@hp"


let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let rec fv_exp = function
  | Nop | Consti(_) | Constf(_) | GetGlobal(_) 
  | FunTableIndex(_) | ExtArray(_) -> []
  | Loadi(x, _)  | Loadf(x, _) | SetGlobal(x, _)
  | Var(x) -> [x]
  | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y)
  | Shl(x, y)
  | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) 
  | Storei(x, y, _) | Storef(x, y, _) -> [x; y]
  | IfEq(_, x, y, e1, e2) | IfLE(_, x, y, e1, e2)
  | IfFEq(_, x, y, e1, e2) | IfFLE(_, x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys) -> x :: ys
  | CallDir(_, ys) -> ys
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

(* all local variables *)
let rec localvs = function
  | Ans(exp) -> []
  | Let(xt, _, e) -> xt :: localvs e

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)
