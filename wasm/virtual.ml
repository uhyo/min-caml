open Asm

let data = ref []
(* Table of type signatures. *)
let type_sigs = ref M.empty
(* Table of functions, used by `call_index`. *)
let func_table = ref []
(* Table of external fuctions. *)
let external_func = ref M.empty

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini
    xts

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
      let offset = align offset in
      (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
      (offset + 4, addi x t offset acc))

(* func_tableに関数名を登録 *)
let register_func_table name ty =
  if List.exists (fun entry -> entry.name = name) !func_table then
    ()
  else
    func_table := ({name; ty}) :: !func_table

(* Conversion of expr. `ty` is the type of current expression *)
let rec g env ty = function
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Consti(i))
  | Closure.Float(d) -> Ans(Constf(d))
  | Closure.Neg(x) ->
      let y = Id.genid "i" in
        Let((y, Type.Int), Consti(0),
            Ans(Sub(V(y), V(x))))
  | Closure.Add(x, y) -> Ans(Add(V(x), V(y)))
  | Closure.Sub(x, y) -> Ans(Sub(V(x), V(y)))
  | Closure.FNeg(x) ->
      let y = Id.genid "f" in
        Let((y, Type.Float), Constf(0.0),
            Ans(FSub(Vf(y), Vf(x))))
  | Closure.FAdd(x, y) -> Ans(FAdd(Vf(x), Vf(y)))
  | Closure.FSub(x, y) -> Ans(FSub(Vf(x), Vf(y)))
  | Closure.FMul(x, y) -> Ans(FMul(Vf(x), Vf(y)))
  | Closure.FDiv(x, y) -> Ans(FDiv(Vf(x), Vf(y)))
  | Closure.IfEq(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfEq(ty, V(x), V(y), g env ty e1, g env ty e2))
      | Type.Float -> Ans(IfFEq(ty, Vf(x), Vf(y), g env ty e1, g env ty e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLE(ty, V(x), V(y), g env ty e1, g env ty e2))
      | Type.Float -> Ans(IfFLE(ty, Vf(x), Vf(y), g env ty e1, g env ty e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env t1 e1 in
      let e2' = g (M.add x t1 env) ty e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) -> Ans(Var(x))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> 
      let e2' = g (M.add x t env) ty e2 in
      let m = Id.genid "m" in (* cls作成後のglobal_hpの値 *)
      let z = Id.genid "l" in (* closureのindex *)
      let offset, store_fv =
        classify
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun (offset, t) y -> 
             (offset + 4, seq(Storef(Vf(y), z, offset), t)))
          (fun (offset, t) y _ -> 
             (offset + 4, seq(Storei(V(y), z, offset), t))) in
      let () = register_func_table l t in
      (* global_hpを取得した後移動させる *)
      Let((x, Type.Int), GetGlobal(global_hp),
          Let((m, Type.Int), Add(V(x), C(offset)),
              seq(SetGlobal(V(m), global_hp),
                  (* 関数のindexを保存 *)
                  Let((z, Type.Int), FunTableIndex(l),
                      seq(Storei(V(z), x, 0),
                          store_fv)))))
  | Closure.AppCls(x, ys) ->
      (* xの型情報を残す *)
      let fty = M.find x env in
      let sigid = "sig." ^ x in
      let () = type_sigs := M.add sigid fty !type_sigs in
        (* グローバル変数にクロージャのアドレスを保存 *)
        seq(SetGlobal(V(x), global_cp),
            Ans(CallCls(x, sigid, ys)))
  | Closure.AppDir(Id.L(x), ys) ->
      (* [XXX] ad-hoc detection of external function *)
      if 9 <= String.length x && "min_caml_" = String.sub x 0 9 then
        begin
          let f_ty = Type.Fun(
            List.map (fun y -> M.find y env) ys,
            ty) in
          external_func := M.add x f_ty !external_func;
        end;
      Ans(CallDir(Id.L(x), ys))
  | Closure.Tuple(xs) ->
      let y = Id.genid "t" in (* tupleの位置 *)
      let m = Id.genid "m" in (* 保存後のglobal_hp *)
      let (offset, store) =
        classify
          (List.map (fun x -> (x, M.find x env)) xs)
          (0, Ans(Var(y)))
          (fun (offset, store) x ->
             (offset + 4,
              seq(Storef(Vf(x), y, offset), store)))
          (fun (offset, store) x _ ->
             (offset + 4,
              seq(Storei(V(x), y, offset), store)))  in
        Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)),
            GetGlobal(global_hp),
            Let((m, Type.Int), Add(V(y), C(offset)),
                seq(SetGlobal(V(m), global_hp),
                    store)))
  | Closure.LetTuple(xts, y, e2) ->
      let s = Closure.fv e2 in
      let (_, load) =
        classify
          xts
          (0, g (M.add_list xts env) ty e2)
          (fun (offset, load) x ->
             (offset+4,
              if not (S.mem x s) then load else
                Let((x, Type.Float),
                    Loadf(y, offset),
                    load)))
          (fun (offset, load) x t ->
             (offset+4,
              if not (S.mem x s) then load else
                Let((x, t),
                    Loadi(y, offset),
                    load))) in
      load
  | Closure.Get(x, y) ->
      let offset = Id.genid "o" in
      let addr = Id.genid "m" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
          Let((offset, Type.Int), Shl(V(y), C(2)),
              Let((addr, Type.Int), Add(V(x), V(offset)),
                  Ans(Loadf(addr, 0))))
      | Type.Array(_) ->
          Let((offset, Type.Int), Shl(V(y), C(2)),
              Let((addr, Type.Int), Add(V(x), V(offset)),
                  Ans(Loadi(addr, 0))))
      | _ -> assert false)
  | Closure.Put(x, y, z) ->
      let offset = Id.genid "o" in
      let addr = Id.genid "m" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
          Let((offset, Type.Int), Shl(V(y), C(2)),
              Let((addr, Type.Int), Add(V(x), V(offset)),
                  Ans(Storef(Vf(z), addr, 0))))
      | Type.Array(_) ->
          Let((offset, Type.Int), Shl(V(y), C(2)),
              Let((addr, Type.Int), Add(V(x), V(offset)),
                  Ans(Storei(V(z), addr, 0))))
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) -> Ans(ExtArray(Id.L("min_caml_" ^ x)))

(* Closure to function definition. *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  match t with
  | Type.Fun(_, t2) ->
      let body = g (M.add x t (M.add_list yts (M.add_list zts M.empty))) t2 e in
        (* Load free variables from closure. *)
      let body =
        if zts = [] then body
        else
          begin
            let p = Id.genid "m" in (* closure pointer *)
            (* load free variables from memory. *)
            let (_, b) =
              classify
                zts
                (4, body)
                (fun (offset, e2) z ->
                   (offset + 4, Let((z, Type.Float), Loadf(p, offset), e2)))
                (fun (offset, e2) z t ->
                   (offset + 4, Let((z, t), Loadi(p, offset), e2))) in
              (* firstly, load closure pointer from the global variable. *)
              Let((p, Type.Int), GetGlobal(global_cp), b)
          end in
        { name = Id.L(x); args = yts; body = body; ret = t2 }
  | _ -> assert false

let f (Closure.Prog(fundefs, e)) =
  func_table := [];
  external_func := M.empty;
  type_sigs := M.empty;
  (* function definitions *)
  let fundefs = List.map h fundefs in
  (* main function *)
  let mainl = Id.genid "main" in
  let mainf = h {
    Closure.name = (Id.L(mainl), Type.Fun([], Type.Unit));
    Closure.args = [];
    Closure.formal_fv = [];
    Closure.body = e;
  } in
  let fundefs = mainf :: fundefs in
  let externals =
    List.map (fun (x, t) -> (Id.L(x), t)) @@
    M.bindings !external_func in
  let typesigs = M.bindings !type_sigs in
    {
      typesigs = typesigs;
      funtable = !func_table;
      fundefs = fundefs;
      externals = externals;
      start = Id.L(mainl);
    }
