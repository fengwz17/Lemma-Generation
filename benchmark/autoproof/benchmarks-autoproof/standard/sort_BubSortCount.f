Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Pairbl = blpair {Bool, List} | sympairbl Int;

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

bubble = 
fix (
lambda f:List->Pairbl.
lambda x:List.
    match x with
        nil _ -> blpair {false, x}
      | cons {y, z} -> 
            match z with
                nil _ -> blpair {false, x}
              | cons {y2, xs} ->
                    if (<= (_to_int y) (_to_int y2))
                    then match (f z) with blpair {b12, ys12} -> blpair {b12, (cons {y, ys12})} end
                    else match (f (cons {y, xs})) with blpair {b1, ys1} -> blpair {true, cons {y2, ys1}} end
            end
    end
);

bubsort =
fix (
lambda f:List->List.
lambda x:List.
    match (bubble x) with
        blpair {c, ys1} ->
            if c then (f ys1) else x
    end
);

lhs = \x:Iint. \xs:List. count (bubsort xs) x;
rhs = \x:Iint. \xs:List. count xs x;