Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Int, List} | symlist Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match m with
  cons {h, t} -> cons {h, (f l t)}
| nil _ -> l
end
);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end);

lhs = \x: List. nat_to_int (length (append (cons {1, nil unit}) x));
rhs = \x: List. nat_to_int (succ (length x));
