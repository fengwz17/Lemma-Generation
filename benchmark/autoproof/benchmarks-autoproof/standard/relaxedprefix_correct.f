Inductive It = a Unit | b Unit | c Unit | symit Int;
Inductive Listit = nilit Unit | consit {It, Listit} | symlistit Int;
Inductive Llistit = lnilit Unit | lconsit {Listit, Llistit} | symllistit Int;
Inductive Blist = nilb Unit | consb {Bool, Blist} | symblist Int;

iteq = 
(
lambda x:It.
lambda y:It.
    match x with 
        a _ -> 
            match y with
                a _ -> true
              | _ -> false
            end
      | b _ ->
            match y with
                b _ -> true
              | _ -> false
            end
      | c _ -> 
            match y with
                c _ -> true
              | _ -> false
            end
    end 
);

removeone = 
fix (
lambda f:It->Llistit->Llistit.
lambda x:It.
lambda y:Llistit.
    match y with 
        lnilit _ -> y
      | lconsit {z, x2} -> lconsit {(consit {x, z}), (f x x2)}
    end
);

removeone2 = 
fix (
lambda f:Listit->Llistit.
lambda x:Listit.
    match x with 
        nilit _ -> (lnilit unit)
      | consit {y, xs} -> lconsit {xs, (removeone y (f xs))}
    end
);

or2 = 
fix (
lambda f:Blist->Bool.
lambda x:Blist.
    match x with
        nilb _ -> false
      | consb {y, xs} -> (or y (f xs))
    end
); 


isprefix = 
fix (
lambda f:Listit->Listit->Bool.
lambda x:Listit.
lambda y:Listit.
    match x with
        nilit _ -> true
      | consit {z, x2} ->
            match y with 
                nilit _ -> false
              | consit {x3, x4} -> (and (iteq z x3) (f x2 x4))
            end
    end
);

isrelaxedprefix = 
fix (
lambda f:Listit->Listit->Bool.
lambda x:Listit.
lambda y:Listit.
    match x with
        nilit _ -> true
      | consit {z,x2} ->
            match x2 with
                nilit _ -> true
              | consit {x3, x4} ->
                    match y with
                        nilit _ -> false
                      | consit {x5, x6} -> 
                            if (iteq z x5) then (f x2 x6) else (isprefix x2 y)
                    end
            end
    end 
);

spec =
fix (
lambda f:Listit->Llistit->Blist.
lambda ys:Listit.
lambda x:Llistit.
    match x with
        lnilit _ -> nilb unit
      | lconsit {y, z} -> consb {(isprefix y ys), (f ys z)}
    end
);

spec2 = (
lambda x:Listit.
lambda y:Listit.
    or2 (spec y (lconsit {x, (removeone2 x)}))
);


lhs = \xs:Listit. \ys:Listit. isrelaxedprefix xs ys;
rhs = \xs:Listit. \ys:Listit. spec2 xs ys;

