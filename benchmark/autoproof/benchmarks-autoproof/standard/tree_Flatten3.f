Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Tree = node {Tree, Iint, Tree} | tnil Unit | symtree Int;
Inductive Ltree = nilt Unit | const {Tree, Ltree} | symltree Int;

x = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);

flatten0 = 
fix (
lambda f:Tree->List.
lambda y:Tree.
    match y with
        tnil _ -> nil unit
      | node {p, z, q} -> (x (f p) (x (cons {z, (nil unit)}) (f q)))
    end
);

flatten3 = 
fix (
lambda f:Tree->List.
lambda y:Tree.
    match y with
        tnil _ -> nil unit
      | node {z, z2, r} -> 
            match z with
                tnil _ -> cons {z2, (f r)}
              | node {p, x2, q} -> (f (node {p, x2, (node {q, z2, r})}))
            end
    end
);

lhs = \p:Tree. flatten3 p;
rhs = \p:Tree. flatten0 p;
