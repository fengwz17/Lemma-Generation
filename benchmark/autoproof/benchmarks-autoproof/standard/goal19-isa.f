Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;


plus = fix (lambda f: Nat -> Nat -> Nat.
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

less = (lambda a: Iint. lambda b: Iint.
(< (_to_int a) (_to_int b))
);

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> y
| succ x0 -> succ (f x0 y)
end);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);

minus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
lambda y: Nat.
match x with
  zero _ -> zero unit
| succ x0 -> 
    match y with
          zero _ -> (succ x0)
        | succ y0 -> (f x0 y0)
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

lhs = \n:Nat. \xs:List. length (drop n xs);
rhs = \n:Nat. \xs:List. minus (length xs) n;