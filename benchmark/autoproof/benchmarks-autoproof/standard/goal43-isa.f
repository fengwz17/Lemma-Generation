Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;


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

takeWhile = 
fix (lambda f: (Iint -> Bool) -> List -> List. lambda p:(Iint -> Bool). lambda l:List.
    match l with
        nil _ -> nil unit 
      | cons {h,t} -> if (p h) then (cons {h, (f p t)}) else (nil unit)
    end
);

dropWhile =
fix (lambda f: (Iint -> Bool) -> List -> List. lambda p: Iint -> Bool. lambda l:List.
    match l with
        nil _ -> nil unit 
      | cons {h,t} -> if (p h) then (f p t) else (cons {h,t})
    end
);

lhs = \xs:List. \p:Iint -> Bool. append (takeWhile p xs) (dropWhile p xs);
rhs = \n:Nat. \x:Nat. \xs: List. xs;