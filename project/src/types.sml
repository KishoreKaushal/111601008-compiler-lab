structure Types =
struct

	type unique = unit ref

	datatype ty = INT
				| STRING
				| BOOL
				| ARRAY of ty * unique

	fun printTy ty =
		case ty of 
				  INT => print "type is int\n"
				| STRING => print "type is string\n"
				| BOOL => print "type is bool\n"
				| ARRAY(arrTy, _) => (print "array: "; printTy ty)
end