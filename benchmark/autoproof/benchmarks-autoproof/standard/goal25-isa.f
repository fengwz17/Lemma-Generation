Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);


less = (lambda a: Iint. lambda b: Iint.
(< (_to_int a) (_to_int b))
);

nmax = (lambda a:Nat. lambda b:Nat.
  if (< (nat_to_int a) (nat_to_int b)) then b else a
);

leq = (lambda x:Nat. lambda y:Nat.
  if (<= (nat_to_int x) (nat_to_int y)) then true else false
);

lhs = \a:Nat. \b:Nat. (== (nat_to_int (nmax a b)) (nat_to_int b));
rhs = \a:Nat. \b:Nat. leq a b;