Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match m with
  cons {h, t} -> cons {h, (f l t)}
| nil _ -> l
end
);


snoc = 
fix (lambda f: Nat -> List -> List. lambda m: Nat. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc h (f t)
| nil _ -> nil unit
end
);

id = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> cons {h, t}
| nil _ -> nil unit
end
);

lhs = \l0: List. rev (append (nil unit) (rev l0));
rhs = \l0: List. l0;
