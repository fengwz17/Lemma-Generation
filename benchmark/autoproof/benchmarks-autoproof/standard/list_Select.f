Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Pair = cpair {Iint, List} | sympair Int;
Inductive Alist = nilp Unit | consp {Pair, Alist} | symalist Int;

select = 
fix (
lambda f:Iint -> Alist -> Alist.
lambda x:Iint.
lambda y:Alist.
    match y with
        nilp _ -> y
      | consp {z, x2} ->
        match z with
            cpair {y2, ys} -> consp {cpair {y2, (cons {x,ys})}, (f x x2)}
        end
    end
);

select2 =
fix (
lambda f:List -> Alist.
lambda x:List.
    match x with
        nil _ -> nilp unit
      | cons {y, xs} -> consp {(cpair {y, xs}), (select y (f xs))}
    end
);

p = 
(lambda x:Pair.
    match x with
        cpair {x, l} -> x
    end
);

lmap =
fix (lambda f: (Pair -> Iint) -> Alist -> List. 
lambda lf: Pair-> Iint. 
lambda l:Alist.
    match l with
        nilp _ -> nil unit
      | consp {h, t} -> cons {(lf h), (f lf t)}
    end
);


lhs = \xs:List. lmap p (select2 xs);
rhs = \xs:List. xs;