Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Int, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, f m t}
| nil _ -> m
end
);

half = fix (lambda f: Nat -> Nat.
lambda x: Nat.
match x with
  zero _ -> zero unit
| succ (zero _) -> zero unit
| succ (succ y0) -> succ (f y0)
end);

len = fix (lambda f: List -> Nat.
lambda l: List.
match l with
  nil _ -> zero unit
| cons {h, t} -> succ (f t)
end);

lhs = \x: List. \y: List. half (len (append x y));
rhs = \x: List. \y: List. half (len (append y x));