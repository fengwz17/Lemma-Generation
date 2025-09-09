Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

nat_to_int = fix (
  \f: Nat -> Int.
  \n: Nat.
  match n with
    zero _ -> 0
  | succ n0 -> + 1 (f n0)
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
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> append (cons {h, nil unit}) (f t)
| nil _ -> nil unit
end
);

lhs = \l0: List. \l1: List. nat_to_int (length (rev (append l0 l1)));
rhs = \l0: List. \l1: List. + (nat_to_int (length l0)) (nat_to_int (length l1));
