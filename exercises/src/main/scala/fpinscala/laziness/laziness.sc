import fpinscala.laziness.prova._

//val emptyStream = Stream.empty
//
//
//
//
//
//val fullStream = Stream.cons(10, Stream.empty)
//
//
//
//
//
//val doubleStream = Stream.cons(9, Stream.cons(10, Stream.empty))
//
//
//
//
//
//
////fullStream.isEmpty
////fullStream.toList
////val longStream = Stream.cons(10, Stream.cons(11, Stream.cons(12, Stream.cons(13,Stream.empty))))
////
////longStream.toList
////longStream.take(2).toList
////longStream.takeWithFold(2)
////
////
////longStream.takeWhile(_ < 12).toList
////longStream.takeWhileWithFold(_ < 12).toList
//Stream.consts("diocane").take(10).toList
//
//
//
//Stream.from(10).take(5).toList
//
//
//Stream.fibs.take(10).toList
//
////Stream.unfoldUberto()[Int, Int](0)( a => Some(a * 2, a + 1) ).take(10).toList
//Stream.unfold[Int, Int](0)( a => Some(a * 2, a + 1) ).take(10).toList
//
//case class FibonacciState(next: Int, afterNext: Int)  {
//  def nextState = FibonacciState(afterNext, next + afterNext)
//}
//
//Stream.unfold[Int, FibonacciState](FibonacciState(0, 1))( fibonacciState => Some(fibonacciState.next, fibonacciState.nextState) ).take(10).toList
//
//
//val stefanoStream = Stream.consts("diocane")
//
//
//
//
//stefanoStream.mapOrig((a: String) => a.length).take(2).toList
//stefanoStream.map((a: String) => a.length).take(2).toList
//
//
////Stream.unfold[Int, MapState](FibonacciState(0, 1))( fibonacciState => Some(fibonacciState.next, fibonacciState.nextState) ).take(10).toList
//
//
//val myzip = Stream.zip(Stream.consts("porco"), Stream.consts("dio")).map( { case (a,b) => a + " " + b } ).take(5).toList
//
//
//
//Stream.startsWithStefano(Stream(1,2,3), Stream(1,2))
//Stream.startsWithStefano(Stream(1,2,3), Stream(2))
//Stream.startsWithStefano(Stream(1,2,3), Stream(1))
//Stream.startsWithStefano(Stream(1,2,3), Stream(2,1,2,3))
//Stream.startsWithStefano(Stream(1,2,3,31,2,23,31,2,3), Stream(1))

Stream.startsWithStefano(Stream(1,2,3), Stream(1,2,3, 4))
Stream.startsWithStefano(Stream(1,2,3), Stream(1,2))



//Stream.zip(Stream(1,2,3,31,2,23,31,2,3), Stream(1,3,5,12)).toList

//Stream(1,2,3,31,2,23,31,2,3).forAll(_ > 0)
//Stream(2,3,31,2,23,31,2,3).forAll(_ > 1)