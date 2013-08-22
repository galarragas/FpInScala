import fpinscala.errorhandling.stefano._

val myLeft : Either[String, Int] = Left("Invalid ID")
val myRight : Either[String, Int] = Right(100)

val listOfEither = List(myLeft, myRight)



val inc: (Int) => Int = _ + 1

myLeft map inc
myRight map inc

myLeft flatMap { value => Right(inc(value)) }
listOfEither map {currEither => currEither map inc}



for {
  currElem <- listOfEither
} yield for(myVal <- currElem) yield inc(myVal)


for {
  currElem <- listOfEither
} yield currElem map inc


