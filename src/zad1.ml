(* Zad 1 *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lrepeat (n, llx)=
    let rec lrep(n, x, lazyRest) =
        if n > 1 then LCons(x, function () -> (lrep(n-1, x, lazyRest)))
        else LCons(x, lazyRest) in

    match llx with
    | LNil -> LNil
    | LCons(x, xf) -> lrep(n, x, function () -> (lrepeat(n, xf())));;

let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

ltake(15,(lrepeat(3,(lfrom 3))));;

