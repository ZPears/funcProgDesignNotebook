// Streams are similar to lists, but their tail is evaluated only on demand.

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

(1 to 1000).toStream

xs.tail

