type 'a t = 'a array array

val make : int -> int -> 'a -> 'a t

val map : ('b -> 'a) -> 'a t -> 'a t

val add, (<+>) : 'a t -> 'a t -> 'a t
val mul, (<*>) : 'a t -> 'a t -> 'a t

val transpose : 'a t -> 'a t

val determinant, det : 'a t -> 'a

val vertical_compose  , (<|>) : 'a t -> 'a t -> 'a t
val horizontal_compose, (<->) : 'a t -> 'a t -> 'a t

val pivoting : 'a t -> 'a t

