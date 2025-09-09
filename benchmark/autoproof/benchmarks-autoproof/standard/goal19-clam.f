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

lhs = \x: List. \y: List. append y (rev (rev x));
rhs = \x: List. \y: List. rev (rev (append y x));
