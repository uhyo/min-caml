open Asm

type imm =
  | Ii of int
  | If of float
  | Iv of Id.t

let rec g env = function
  | Ans(exp) ->
      let (exp', s, r) = g' env exp in
        (Ans(exp'), s, r)
  | Let((x, t), ((Consti(i)) as exp), e2) ->
      let env' = M.add x (Ii(i)) env in
      let (exp', _, _) = g' env exp in
      let (e2', s, r2) = g env' e2 in
        if S.mem x s then
          (Let((x, t), exp', e2'), S.remove x s, r2)
        else
          (e2', s, r2)
  | Let((x, t), ((Constf(f)) as exp), e2) ->
      let env' = M.add x (If(f)) env in
      let (exp', _, _) = g' env exp in
      let (e2', s, r2) = g env' e2 in
        if S.mem x s then
          (Let((x, t), exp', e2'), S.remove x s, r2)
        else
          (e2', s, r2)
  | Let((x, t), ((Var(y)) as exp), e2) ->
      let env' = M.add x (Iv(y)) env in
      let (exp', _, _) = g' env exp in
      let (e2', s, r2) = g env' e2 in
        if S.mem x s then
          (Let((x, t), exp', e2'), s, r2)
        else
          (e2', s, r2)
  | Let((x, t), exp, e2) ->
      let (exp', s1, r1) = g' env exp in
      let (e2', s2, r2) = g env e2 in
        if S.mem x s2 || (not r1) then
          (Let((x, t), exp', e2'), S.remove x (S.union s1 s2), r1 && r2)
        else
          (e2', s2, r1 && r2)
and g' env = function
  | Nop
  | Consti(_)
  | Constf(_)
      as t -> (t, S.empty, true)
  | Add(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Add(x', y'), S.union s1 s2, true)
  | Sub(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Sub(x', y'), S.union s1 s2, true)
  | Mul(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Mul(x', y'), S.union s1 s2, true)
  | Div(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Div(x', y'), S.union s1 s2, true)
  | Shl(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Shl(x', y'), S.union s1 s2, true)
  | Eq(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Eq(x', y'), S.union s1 s2, true)
  | LE(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (LE(x', y'), S.union s1 s2, true)
  | FAdd(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FAdd(x', y'), S.union s1 s2, true)
  | FSub(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FSub(x', y'), S.union s1 s2, true)
  | FMul(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FMul(x', y'), S.union s1 s2, true)
  | FDiv(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FDiv(x', y'), S.union s1 s2, true)
  | FEq(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FEq(x', y'), S.union s1 s2, true)
  | FLE(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FLE(x', y'), S.union s1 s2, true)
  | Loadi(x, _)
  | Loadf(x, _) as t -> (t, S.singleton x, true)
  | Storei(v, x, c) ->
      let (v', s) = gimm env v in
        (Storei(v', x, c), S.add x s, false)
  | Storef(v, x, c) ->
      let (v', s) = gimmf env v in
        (Storef(v', x, c), S.add x s, false)
  | GetGlobal(_) as t -> (t, S.empty, true)
  | SetGlobal(v, x) ->
      let (v', s) = gimm env v in
      (SetGlobal(v', x), S.add x s, false)
  | If(ty, x, e1, e2) ->
      let (x', sc) = gimm env x in
      let (e1', s1, r1) = g env e1 in
      let (e2', s2, r2) = g env e2 in
      (If(ty, x', e1', e2'), S.union s1 (S.union s2 sc), r1 && r2)
  | CallCls(x, y, args, fargs) ->
      let (args', s) =
        List.fold_right
          (fun z (args', s) ->
             let (z', s1) = gimm env z in
               (z'::args', S.union s1 s))
          args
          ([], S.empty) in
      let (fargs', s) =
        List.fold_right
          (fun z (fargs', s) ->
             let (z', s1) = gimmf env z in
               (z'::fargs', S.union s1 s))
          fargs
          ([], s) in
        (CallCls(x, y, args', fargs'), s, false)
  | CallDir(x, args, fargs) ->
      let (args', s) =
        List.fold_right
          (fun z (args', s) ->
             let (z', s1) = gimm env z in
               (z'::args', S.union s1 s))
          args
          ([], S.empty) in
      let (fargs', s) =
        List.fold_right
          (fun z (fargs', s) ->
             let (z', s1) = gimmf env z in
               (z'::fargs', S.union s1 s))
          fargs
          ([], s) in
        (CallDir(x, args', fargs'), s, false)
  | Var(x) when M.mem x env ->
      begin
        match M.find x env with
          | Ii(i) -> (Consti(i), S.empty, true)
          | If(f) -> (Constf(f), S.empty, true)
          | Iv(y) -> (Var(y), S.singleton y, true)
      end
  | Var(x) as t -> (t, S.singleton x, true)
  | FunTableIndex(_) as t -> (t, S.empty, true)
  | ExtArray(_) as t -> (t, S.empty, true)
and gimm env = function
  | V(x) when M.mem x env ->
      begin
        match M.find x env with
          | Ii(i) -> (C(i), S.empty)
          | Iv(y) -> (V(y), S.empty)
          | _ -> assert false
      end
  | V(x) as v -> (v, S.singleton x)
  | v -> (v, S.empty)
and gimmf env = function
  | Vf(x) when M.mem x env ->
      begin
        match M.find x env with
          | If(i) -> (F(i), S.empty)
          | Iv(y) -> (Vf(y), S.empty)
          | _ -> assert false
      end
  | Vf(x) as v -> (v, S.singleton x)
  | v -> (v, S.empty)

let h { name = l; args; fargs; body = e; ret = t } =
  let (e', _, _) = g M.empty e in
  { name = l; args; fargs; body = e'; ret = t }

let f prog =
  {
    typesigs = prog.typesigs;
    funtable = prog.funtable;
    fundefs = List.map h prog.fundefs;
    externals = prog.externals;
    start = prog.start;
  }
