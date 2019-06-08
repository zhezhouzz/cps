signature CPS_TRANS =
sig
    include DSL_AST
    val cpsTrans : top_level -> top_level
end

functor CpsTrans(DslAst : DSL_AST) : CPS_TRANS =
struct
open DslAst
open Atoms
fun cpsTrans exp =
    case exp of
        Var id =>
        let
            val k = Id.new ()
        in
            Abs(k, App (Var k, Var id))
        end
     | Abs (id, e1) =>
       let
           val e1 = cpsTrans e1
           val k = Id.new ()
       in
           Abs(k, App (Var k, Abs(id, e1)))
       end
     | App (e1, e2) =>
       let
           val e1 = cpsTrans e1
           val e2 = cpsTrans e2
           val k = Id.new ()
           val ke1 = Id.new ()
           val ke2 = Id.new ()
       in
           Abs(k,
               App(e1,
                   Abs(ke1,
                       App(e2,
                           Abs(ke2,
                               App (App (Var ke1, Var ke2), Var k)
                              )
                          )
                   )
                  )
              )
       end
end
