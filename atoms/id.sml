signature NAME_SPACE =
sig
    val nameSpace :
        {idHashtbl : (int, string) HashTable.hash_table,
         strHashtbl : (string, int) HashTable.hash_table,
         curId : int ref}
    val initSpace : unit -> unit
end

structure NameSpace : NAME_SPACE =
struct
fun stringEq (s1, s2) =
    case String.compare (s1, s2) of
        EQUAL => true
      | _ => false

fun newNameSpace () =
    {idHashtbl = HashTable.mkTable ((MLton.hash, op=), 0),
     strHashtbl = HashTable.mkTable ((MLton.hash, stringEq), 0),
     curId = ref 0}

val nameSpace = newNameSpace ()

fun initSpace () =
    case nameSpace of
        {idHashtbl, strHashtbl, curId} =>
        let
            val _ = HashTable.clear idHashtbl
            val _ = HashTable.clear strHashtbl
            val _ = curId := 0
        in
            ()
        end
end

signature ID =
sig
    include NAME_SPACE
    type t
    val register: string -> t
    val layout: t -> string
    val new: unit -> t
end

functor Id (NameSpace: NAME_SPACE) : ID =
struct
open NameSpace
type t = int

fun register str =
    case nameSpace of
        {idHashtbl, strHashtbl, curId} =>
        (case HashTable.find (strHashtbl, str) of
             SOME id => id
           | NONE =>
             let
                 val _ = HashTable.insert (idHashtbl, ((!curId), str))
                 val _ = HashTable.insert (strHashtbl, (str, (!curId)))
                 val id = !curId
                 val _ = curId := (id + 1)
             in
                 id
             end)

fun layout id =
    case nameSpace of
        {idHashtbl, strHashtbl, curId} =>
        HashTable.lookup (idHashtbl, id)

fun new () =
    case nameSpace of
        {idHashtbl, strHashtbl, curId} =>
        let
            fun goodName i =
                let
                    val name = "k" ^ (Int.toString i)
                in
                    case HashTable.find (strHashtbl, name) of
                        NONE => name
                      | _ => goodName (i + 1)
                end
            val name = goodName (!curId)
            val id = register name
        in
            id
        end
end
