Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Pair = cpair {Iint, Iint} | sympair Int;
Inductive Zlist = znil Unit | zcons {Pair, Zlist} | symzlist Int;


zip =
fix (lambda f:List -> List -> Zlist. lambda l1:List. lambda l2:List.
    match l1 with
        nil _ -> znil unit
      | cons {h1, t1} -> match l2 with
                            nil _ -> znil unit 
                          | cons {h2, t2} -> zcons {cpair {h1, h2}, (f t1 t2)}
                         end
    end
);

lhs = \ys:List. zip (nil unit) ys;
rhs = \ys:List. znil unit;