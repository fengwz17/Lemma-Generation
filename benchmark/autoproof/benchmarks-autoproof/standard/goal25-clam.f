Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Int, List} | symlist Int;

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
| cons {h1, t} -> succ (f t)
end);


plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

lhs = \x: List. \y: List. even (len (append x y));
rhs = \x: List. \y: List. even (plus (len x) (len y));