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

flatten1 = 
fix (
lambda f:Ltree->List.
lambda y:Ltree.
    match y with
        nilt _ -> nil unit
      | const {z, ps} -> 
            match z with
                tnil _ -> (f ps)
              | node {z2, x2, q} ->
                    match z2 with
                        tnil _ -> cons {x2, (f (const {q, ps}))}
                      | node {x3, x4, x5} -> (f (const {z2, (const {node {(tnil unit), x2, q}, ps})}))
                    end
            end
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

lhs = \p:Tree. flatten1 (const {p, (nilt unit)});
rhs = \p:Tree. flatten0 p;
