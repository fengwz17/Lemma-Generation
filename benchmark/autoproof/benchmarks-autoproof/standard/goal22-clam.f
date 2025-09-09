Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, f m t}
| nil _ -> m
end
);

even = fix (lambda f: Nat -> Bool.
lambda x: Nat.
match x with
  zero _ -> true
| succ y0 -> not (f y0)
end);

len = fix (lambda f: List -> Nat.
lambda l: List.
match l with
  nil _ -> zero unit
| cons {h, t} -> succ (f t)
end);

lhs = \x: List. \y: List. even (len (append x y));
rhs = \x: List. \y: List. even (len (append y x));