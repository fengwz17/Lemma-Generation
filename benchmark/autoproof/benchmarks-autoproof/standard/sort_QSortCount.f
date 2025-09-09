Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

eq = (lambda a: Iint.
lambda b: Iint.
if (== (_to_int a) (_to_int b)) then 1 else 0
);

count = 
fix (lambda f: List -> Iint -> Int.
lambda l: List.
lambda x: Iint.
match l with
  cons {h, t} -> + (eq h x) (f t x)
| nil _ -> 0
end
);

leq = (lambda x:Iint. lambda y:Iint.
  if (<= (_to_int x) (_to_int y)) then true else false
);

gt = 
(
lambda x:Iint.
lambda y:Iint.
    leq y x
);


length = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> + 1 (f t)
| nil _ -> 0
end
);

x = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);


filter =
fix (lambda f: (Iint -> Bool) -> List -> List. lambda p: Iint -> Bool. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> if (p h) then cons {h, (f p l)} else (f p l)
end
);

qsort = 
fix (
lambda f:List->List.
lambda y:List.
    match y with
        nil _ -> y
      | cons {z, xs} -> 
            x (f (filter (\z2:Iint. (leq z2 z)) xs))
            (x (cons {z, (nil unit)}) (f (filter (\x2:Iint. (gt x2 z)) xs)))
    end
);

lhs = \x:Iint. \xs:List. (count (qsort xs) x);
rhs = \x:Iint. \xs:List. count xs x;

