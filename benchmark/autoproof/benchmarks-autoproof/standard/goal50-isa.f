Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);

isnil = 
(lambda l:List.
    match l with
        nil _ -> true
      | cons _ -> false
    end
);

butlast = 
fix (lambda f:List -> List. lambda l:List.
    match l with
        nil _ -> nil unit 
      | cons {h, t} -> if (isnil t) then (nil unit) else (cons {h, (f t)})
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

lhs = \xs:List.  butlast xs;
rhs = \xs:List.  take (minus (length xs) (succ (zero unit))) xs;