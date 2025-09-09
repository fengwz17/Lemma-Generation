Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);

take =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match n with
    zero _ -> nil unit
  | succ n0 -> 
    match l with
        cons {h, t} -> cons {h, (f n0 t)}
    |   nil _ -> nil unit
    end
end
);

drop =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match n with
    zero _ -> l
  | succ n0 -> 
    match l with
        cons {h, t} -> (f n0 t)
    |   nil _ -> nil unit
    end
end
);

lmap =
fix (lambda f: (Iint -> Iint) -> List -> List. lambda lf: Iint-> Iint. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> cons {(lf h), (f lf t)}
end
);

lhs = \n:Nat. \xs: List. \f:Iint -> Iint. take n (lmap f xs);
rhs = \n:Nat. \xs: List. \f:Iint -> Iint. lmap f (take n xs);