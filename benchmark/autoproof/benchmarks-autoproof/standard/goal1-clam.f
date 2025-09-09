Inductive Nat = zero Unit | succ Nat | symnat Int;

double = fix (lambda f: Nat -> Nat.
lambda x: Nat.
match x with
  zero _ -> zero unit
|  succ y -> succ (succ (f y))
end);

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda y: Nat.
lambda x: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f y x0)
end);

lhs = \x: Nat. (double x);
rhs = \x: Nat. (plus x x);
