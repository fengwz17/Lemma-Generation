Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

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

last =
fix (lambda f:List -> Iint. lambda l:List. 
    match l with
        nil _ -> cint 0
      | cons {h, t} -> if (isnil t) then h else (f t)
    end 
);



lhs = \xs:List. \ys:List. last (append xs ys);
rhs = \xs:List. \ys:List. if (isnil ys) then (last xs) else (last ys);