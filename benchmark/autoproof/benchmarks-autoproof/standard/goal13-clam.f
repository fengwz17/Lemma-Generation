Inductive Nat = zero Unit | succ Nat | symnat Int;

half = fix (lambda f: Nat -> Nat.
lambda x: Nat.
match x with
  zero _ -> zero unit
| succ (zero _) -> zero unit
| succ (succ x0)-> succ (f x0)
end
);

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

lhs = \x: Nat. half (plus x x);
rhs = \x: Nat. x;