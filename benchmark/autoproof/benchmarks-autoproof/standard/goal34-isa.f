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


leq = (lambda x:Nat. lambda y:Nat.
  if (<= (nat_to_int x) (nat_to_int y)) then true else false
);

nmax = (lambda a:Nat. lambda b:Nat.
  if (< (nat_to_int a) (nat_to_int b)) then b else a
);

nmin = (lambda x:Nat. lambda y:Nat.
    if (< (nat_to_int x) (nat_to_int y)) then x else y
);

nateq = (lambda x:Nat. lambda y:Nat.
  if (== (nat_to_int x) (nat_to_int y)) then true else false
);

lhs = \a:Nat. \b:Nat. (nateq (nmin a b) a);
rhs = \a:Nat. \b:Nat. leq a b;