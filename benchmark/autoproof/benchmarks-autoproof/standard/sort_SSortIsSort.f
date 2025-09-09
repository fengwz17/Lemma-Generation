Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

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

eqn =
(
lambda x:Iint.
lambda y:Iint.
    (== (_to_int x) (_to_int y))
);

ssortminimum1 = 
fix (
lambda f:Iint->List->Iint.
lambda x:Iint.
lambda y:List.
    match y with
        nil _ -> x
      | cons {y1, ys1} ->
            if (leq y1 x) then (f y1 ys1) else (f x ys1)
    end
);

deleteBy = 
fix (
lambda f: Iint->(Iint->Bool)->Iint->List->List.
lambda x:Iint->(Iint->Bool).
lambda y:Iint.
lambda z:List.
    match z with
        nil _ -> z
      | cons{y2, ys} ->
            if ((x y) y2) then ys else (cons {y2, (f x y ys)})
    end
);

ssort = 
fix (
lambda f:List->List.
lambda x:List.
    match x with 
        nil _ -> x
      | cons {y, ys} ->
            cons {(ssortminimum1 y ys), (f (deleteBy (\z:Iint. \x2:Iint. (eqn z x2)) (ssortminimum1 y ys) x))}
    end
);


insert = 
fix (lambda f: Iint -> List -> List. lambda n:Iint. lambda l:List.
    match l with
        nil _ -> cons {n, nil unit}
      | cons {h,t} -> if (leq n h) 
                        then cons {n, cons {h, t}} 
                        else (f n t)
    end
);

isort = 
fix (lambda f:List -> List. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> insert h (f t)
end
);

lhs = \x:Iint. \xs:List. ssort xs;
rhs = \x:Iint. \xs:List. isort xs;