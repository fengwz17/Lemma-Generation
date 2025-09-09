Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;
Inductive Llist = cnil List | ccons {Nat, List, Llist} | symLlist Int;


snoc = 
fix (lambda f: Nat -> List -> List. lambda m: Nat. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc h (f t)
| nil _ -> nil unit
end
);

lhs = \l0: List. \l1: List. rev (append l1 (rev l0));
rhs = \l0: List. \l1: List. append l0 (rev l1);
