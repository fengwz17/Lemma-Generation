Inductive Iint = cint Int | err Unit | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Pair = cpair {Iint, Iint} | sympair Int;
Inductive Zlist = znil Unit | zcons {Pair, Zlist} | symzlist Int;


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

head = (lambda l:List.
    match l with
        nil _ -> err unit 
      | cons {h, t} -> h
    end
);

tail = (lambda l:List.
    match l with
        nil _ -> nil unit 
      | cons {h, t} -> t
    end
);

isnil = 
(lambda l:List.
    match l with
        nil _ -> true
      | cons _ -> false
    end
);


lhs = \x:Iint. \xs:List. \ys:List. zip (cons {x, xs}) ys;
rhs = \x:Iint. \xs:List. \ys:List. if (isnil ys) then (znil unit) else (zcons {cpair {x, (head ys)}, (zip xs (tail ys))});