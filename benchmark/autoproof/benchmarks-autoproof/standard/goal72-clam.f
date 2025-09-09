Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

lhs = \l0: List. \l1: List. \l2: List. append l0 (append l1 l2);
rhs = \l0: List. \l1: List. \l2: List. append (append l0 l1) l2;