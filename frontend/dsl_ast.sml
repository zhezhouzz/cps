signature DSL_AST =
sig
    structure Atoms: ATOMS
    datatype exp =
             Var of Atoms.Id.t
             | Abs of Atoms.Id.t * exp
             | App of exp * exp

    type top_level = exp
    val varToId: string -> Atoms.Id.t
    val layout : top_level -> string
end

functor DslAst (structure Atoms : ATOMS) : DSL_AST =
struct
structure Atoms = Atoms

datatype exp =
         Var of Atoms.Id.t
         | Abs of Atoms.Id.t * exp
         | App of exp * exp

type top_level = exp

open Atoms

open Format

val varToId = Id.register

fun layoutExp exp =
    case exp of
        Var id => (Id.layout id, false)
      | Abs (id, e1) =>
        let
            val (e1Layout, hasSpace) = layoutExp e1
            val e1Layout = if hasSpace then "(" ^ (e1Layout) ^ ")" else e1Layout
        in
            ("\\" ^ (Id.layout id) ^ "." ^ e1Layout, false)
        end
      | App (e1, e2) =>
        let
            val (e1Layout, hasSpace) = layoutExp e1
            val e1Layout = if hasSpace then "(" ^ (e1Layout) ^ ")" else e1Layout
            val (e2Layout, hasSpace) = layoutExp e2
            val e2Layout = if hasSpace then "(" ^ (e2Layout) ^ ")" else e2Layout
        in
            ("@" ^ e1Layout ^ " " ^ e2Layout, true)
        end
fun layout exp =
    let
        val (str, _) = layoutExp exp
    in
        str
    end
end
