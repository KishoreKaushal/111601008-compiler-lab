signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string

  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
                  in nextsym := i+1;
                     H.insert hashtable (name,i);
                     (name,i)
		          end

  fun name(s,n) = s

  structure Table = IntMapTable(type key = symbol fun getInt(s,n) = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look
end

(* Usage of IntMapTable Functor *)

(*
    functor IntMapTable (type key
                val getInt: key -> int): TABLE =
    struct

    type key = key
    type 'a table = 'a IntBinaryMap.map
    val empty = IntBinaryMap.empty
    fun enter (t, k, a) = IntBinaryMap.insert (t, getInt k, a)
    fun look (t, k) = IntBinaryMap.find (t, getInt k)
    val numItems = IntBinaryMap.numItems
    fun fromList kas =
        let
            fun fromList1 [] t = t
            | fromList1 ((k, a)::kas) t = enter (fromList1 kas t, k, a)
        in
            fromList1 kas empty
        end

    end
*)