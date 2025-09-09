Inductive Pair = cpair {Iint, Iint} | sympair Int;
Inductive Zlist = znil Unit | zcons {Pair, Zlist} | symzlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

zip =
fix (lambda f:List -> List -> Zlist. lambda l1:List. lambda l2:List.
    match l1 with
        nil _ -> znil unit
      | cons {h1, t1} -> match l2 with
                            nil _ -> znil unit 
                          | cons {h2, t2} -> zcons {cpair {h1, h2}, (f t1 t2)}
                         end
    end
);

take =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match {n, l} with
    {zero _, nil _} -> nil unit
  | {zero _, cons {h, t}} -> nil unit
  | {succ n0, nil _} -> nil unit 
  | {succ n0, cons {h, t}} -> cons {h, f n0 t}
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

zdrop = 
fix (lambda f:Nat -> Zlist -> Zlist. lambda n:Nat. lambda l:Zlist.
    match n with
    zero _ -> l
  | succ n0 -> 
    match l with
        zcons {h, t} -> (f n0 t)
    |   znil _ -> znil unit
    end
end
);

lhs = \n:Nat. \xs:List. \ys:List. zdrop n (zip xs ys);
rhs = \n:Nat. \xs:List. \ys:List. zip (drop n xs) (drop n ys);