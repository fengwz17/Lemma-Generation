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

lhs = \x:Iint. (_to_int x);
rhs = \x:Iint. + (_to_int x) 0;