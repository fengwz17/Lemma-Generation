Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive IInt = cint Int | symiInt Int;
Inductive List = nil Unit | cons {IInt, List} | symlist Int;


append = 
fix (lambda f: List -> List -> List.  lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);


double = fix (lambda f: Nat -> Nat.
lambda x: Nat.
match x with
  zero _ -> zero unit
|  succ y -> succ (succ (f y))
end);

lhs = \l0: List.  (length (append l0 l0));
rhs = \l0: List.  (double (length l0));
