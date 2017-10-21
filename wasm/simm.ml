open Asm

type imm =
  | Ii of int
  | If of float
  | Iv of Id.t

let rec g env = function
  | Ans(exp) ->
      let (exp', s) = g' env exp in
        (Ans(exp'), s)
  | Let((x, t), ((Consti(i)) as exp), e2) ->
      let env' = M.add x (Ii(i)) env in
      let (e2', s) = g env' e2 in
        if S.mem x s then
          (Let((x, t), fst (g' env exp), e2'), S.remove x s)
        else
          (e2', s)
  | Let((x, t), ((Constf(f)) as exp), e2) ->
      let env' = M.add x (If(f)) env in
      let (e2', s) = g env' e2 in
        if S.mem x s then
          (Let((x, t), fst (g' env exp), e2'), S.remove x s)
        else
          (e2', s)
  | Let((x, t), ((Var(y)) as exp), e2) ->
      let env' = M.add x (Iv(y)) env in
      let (e2', s) = g env' e2 in
        if S.mem x s then
          (Let((x, t), fst (g' env exp), e2'), s)
        else
          (e2', s)
  | Let((x, t), exp, e2) ->
      let (exp', s1) = g' env exp in
      let (e2', s2) = g env e2 in
      (Let((x, t), exp', e2'), S.remove x (S.union s1 s2))
and g' env = function
  | Add(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Add(x', y'), S.union s1 s2)
  | Sub(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Sub(x', y'), S.union s1 s2)
  | Mul(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Mul(x', y'), S.union s1 s2)
  | Div(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Div(x', y'), S.union s1 s2)
  | Shl(x, y) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
        (Shl(x', y'), S.union s1 s2)
  | FAdd(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FAdd(x', y'), S.union s1 s2)
  | FSub(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FSub(x', y'), S.union s1 s2)
  | FMul(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FMul(x', y'), S.union s1 s2)
  | FDiv(x, y) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
        (FDiv(x', y'), S.union s1 s2)
  | Storei(v, x, c) ->
      let (v', s) = gimm env v in
        (Storei(v', x, c), S.add x s)
  | Storef(v, x, c) ->
      let (v', s) = gimmf env v in
        (Storef(v', x, c), S.add x s)
  | SetGlobal(v, x) ->
      let (v', s) = gimm env v in
      (SetGlobal(v', x), S.add x s)
  | IfEq(ty, x, y, e1, e2) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
      let (e1', s3) = g env e1 in
      let (e2', s4) = g env e2 in
      (IfEq(ty, x', y', e1', e2'), S.union s1 (S.union s2 (S.union s3 s4)))
  | IfLE(ty, x, y, e1, e2) ->
      let (x', s1) = gimm env x in
      let (y', s2) = gimm env y in
      let (e1', s3) = g env e1 in
      let (e2', s4) = g env e2 in
      (IfLE(ty, x', y', e1', e2'), S.union s1 (S.union s2 (S.union s3 s4)))
  | IfFEq(ty, x, y, e1, e2) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
      let (e1', s3) = g env e1 in
      let (e2', s4) = g env e2 in
      (IfFEq(ty, x', y', e1', e2'), S.union s1 (S.union s2 (S.union s3 s4)))
  | IfFLE(ty, x, y, e1, e2) ->
      let (x', s1) = gimmf env x in
      let (y', s2) = gimmf env y in
      let (e1', s3) = g env e1 in
      let (e2', s4) = g env e2 in
      (IfFLE(ty, x', y', e1', e2'), S.union s1 (S.union s2 (S.union s3 s4)))
  | Var(x) when M.mem x env ->
      begin
        match M.find x env with
          | Ii(i) -> (Consti(i), S.empty)
          | If(f) -> (Constf(f), S.empty)
          | Iv(y) -> (Var(y), S.singleton y)
      end
  | t -> (t, S.of_list (fv_exp t))
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
  { name = l; args; fargs; body = fst (g M.empty e); ret = t }

let f prog =
  {
    typesigs = prog.typesigs;
    funtable = prog.funtable;
    fundefs = List.map h prog.fundefs;
    externals = prog.externals;
    start = prog.start;
  }
