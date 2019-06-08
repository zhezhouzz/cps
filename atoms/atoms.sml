signature ATOMS =
sig
    structure Id : ID
end

structure Atoms : ATOMS =
struct
structure Id = Id(NameSpace)
end


(* open Atoms *)
(* open Id; *)

(* let *)
(*     val id1 = register "x" *)
(*     val id2 = new () *)
(*     val _ = print ((layout id1) ^ "\n") *)
(*     val _ = print ((layout id2) ^ "\n") *)
(* in *)
(*     () *)
(* end; *)
