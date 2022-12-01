(* Zad 3 *)
type 'a lBT = LEmpty | LNode of  'a * (unit ->'a lBT) * (unit -> 'a lBT);;
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

let rec lTree n =
    LNode(n, (function () -> lTree(2*n)), (function () -> lTree(2*n+1)));;

let bfs ltree =
    let rec bfsIn queue =
        match queue with
        | [] -> LNil
        | LEmpty::t -> bfsIn t
        | LNode(v, l, r)::t -> LCons(v, (function () -> bfsIn (t @ l() :: r() :: []))) in
        bfsIn [ltree];;


ltake(7, (bfs (lTree 5)));;
