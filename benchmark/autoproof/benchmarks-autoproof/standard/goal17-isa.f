Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
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

nateq = (lambda x:Nat. lambda y:Nat.
  if (== (nat_to_int x) (nat_to_int y)) then true else false
);


lhs = \n:Nat. (<= (nat_to_int n) 0);
rhs = \n:Nat. (== (nat_to_int n) 0);

