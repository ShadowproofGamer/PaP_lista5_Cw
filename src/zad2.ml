let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;


(*Zad 2*)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;


let lfib =
    let rec lfibIn(p, n) =
        LCons(p+n, lazy(lfibIn(n, p+n))) in
    LCons(1, lazy(LCons(1, lazy(lfibIn(1, 1)))));;

ltake(15, lfib);;


