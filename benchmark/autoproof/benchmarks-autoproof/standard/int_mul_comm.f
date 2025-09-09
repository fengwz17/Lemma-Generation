Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;

x2 = 
fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

plus2 = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

neg = (
lambda x:Iint.
- 0 (_to_int x)
);

lhs = \x:Iint. \y:Iint. * (_to_int x) (_to_int y);
rhs = \x:Iint. \y:Iint. * (_to_int y) (_to_int x);