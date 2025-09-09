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

lhs = \l0: List. rev (rev l0);
rhs = \l0: List. l0;