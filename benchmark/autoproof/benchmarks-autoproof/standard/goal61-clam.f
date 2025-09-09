Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

lhs = \a: Nat. \l0: List. \l1: List. append l1 (append (cons {a, nil unit}) l0);
rhs = \a: Nat. \l0: List. \l1: List. append (cons {a, l1}) l0;