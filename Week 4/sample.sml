
fun double x = 2 * x
fun incr x = x + 1
fun is_even x = x mod 2 = 0

fun n_times(f, n, x) =
  if n = 0
  then x
  else f(n_times(f, n - 1, x))

val a = n_times(double, 10, 2)

fun map(f, s) =
  case s of
    [] => []
    | (x :: xs) => f(x) :: map(f, xs)

val x = map(fn x => 2 * x, [1, 2, 3])

fun filter(f, s) =
  case s of
    [] => []
	| (x :: xs) => if f(x)
				   then x :: filter(f, xs)
				   else filter(f, xs)

val z = filter(fn x => x mod 2 = 0, [1,2,3,4,5,6,7,8,9,0])

infix !>
fun a !> b = b(a)
val y = 2 !> incr !> double


val sorted = fn x => fn y => fn z => z >= y andalso y >= x
fun sorted_1 x y z = x <= y andalso y <= z 

val t1 = sorted 3 7 5
val t2 = sorted_1 3 7 5

fun filter_c f s =
  case s of
    [] => []
	| (x :: xs) => if f(x)
				   then x :: filter(f, xs)
				   else filter(f, xs)

val gtzero = filter_c (fn x => x > 0)
val nozeroes = gtzero [0,1,2,3]