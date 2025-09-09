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

ztake =
fix (lambda f: Nat -> Zlist -> Zlist. lambda n:Nat. lambda l:Zlist.
  match n with
    zero _ -> znil unit
  | succ n0 -> 
    match l with
        zcons {h, t} -> zcons {h, (f n0 t)}
    |   znil _ -> znil unit
    end
end
);

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);

zappend = 
fix (lambda f: Zlist -> Zlist -> Zlist. lambda l: Zlist. lambda m: Zlist. 
match l with
  zcons {h, t} -> zcons {h, (f t m)}
| znil _ -> m
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

lhs = \xs:List. \ys:List. \zs:List. (zip xs (append ys zs));
rhs = \xs:List. \ys:List. \zs:List. zappend (zip (take (length ys) xs) ys) (zip (drop (length ys) xs) zs);