Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Iint = cint Int | symiint Int;

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

lhs = \xs:List. \ys:List. butlast (append xs ys);
rhs = \xs:List. \ys:List. if (isnil ys) then (butlast xs) else (append xs (butlast ys));