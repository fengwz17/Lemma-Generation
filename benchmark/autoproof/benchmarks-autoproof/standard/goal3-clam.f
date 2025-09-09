Inductive IInt = cint Int | symiInt Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {IInt, List} | symlist Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

append = 
fix (lambda f: List -> List -> List.  lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);


lhs = \x: IInt. \l0: List. \l1: List. + (nat_to_int (length l0)) (nat_to_int (length l1));
rhs = \x: IInt. \l0: List. \l1: List. nat_to_int (length (append l0 l1));
