Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Tree = tnode {Tree, Iint, Tree} | tnil Unit | symtree Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

eq = (lambda a: Iint.
lambda b: Iint.
if (== (_to_int a) (_to_int b)) then 1 else 0
);

count = 
fix (lambda f: List -> Iint -> Int.
lambda l: List.
lambda x: Iint.
match l with
  cons {h, t} -> + (eq h x) (f t x)
| nil _ -> 0
end
);

leq = (lambda x:Iint. lambda y:Iint.
  if (<= (_to_int x) (_to_int y)) then true else false
);

gt = 
(
lambda x:Iint.
lambda y:Iint.
    leq y x
);

eqn =
(
lambda x:Iint.
lambda y:Iint.
    (== (_to_int x) (_to_int y))
);

flatten =
fix (
lambda f:Tree->List->List.
lambda x:Tree.
lambda y:List.
    match x with
        tnil _ -> y
      | tnode {q, z ,r} -> (f q (cons {z, (f r y)}))
    end
);

addt =
fix (
lambda f:Iint->Tree->Tree.
lambda x:Iint.
lambda y:Tree.
    match y with 
        tnil _ -> tnode {y, x, y}
      | tnode {q,z,r} -> 
            if (leq x z) then (tnode {(f x q), z, r}) else (tnode {q, z, (f x r)})
    end
);

toTree = 
fix (
lambda f:List->Tree.
lambda x:List.
    match x with
        nil _ -> (tnil unit)
      | cons {y, xs} -> (addt y (f xs))
    end
);

tsort = 
(
lambda x:List.
flatten (toTree x) (nil unit)
);

lhs = \x:Iint. \xs:List. count (tsort xs) x;
rhs = \x:Iint. \xs:List. count xs x;





