type 'a matrix = 'a array array

val map : ('b -> 'a) -> 'a matrix -> 'a matrix

val add, (<+>) : 'a matrix -> 'a matrix -> 'a matrix
val mul, (<*>) : 'a matrix -> 'a matrix -> 'a matrix

val transpose : 'a matrix -> 'a matrix

val determinant, det : 'a matrix -> 'a

val vertical_compose  , (<|>) : 'a matrix -> 'a matrix -> 'a matrix
val horizontal_compose, (<->) : 'a matrix -> 'a matrix -> 'a matrix

val pivoting : 'a matrix -> 'a matrix

