Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> append (cons {h, nil unit}) (f t)
| nil _ -> nil unit
end
);

lhs = \a: Nat. \l0: List. rev (append (append (cons {a, nil unit}) (nil unit)) l0);
rhs = \a: Nat. \l0: List.  cons {a, rev (append (nil unit) l0)};