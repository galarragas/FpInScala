
type PrintedInteger = (Int, Option[String])

type StatefulPrint = Int => PrintedInteger

def id: StatefulPrint = (value => (value, Some(value.toString)))

def fizz: StatefulPrint = ((value) => if ((value % 3) == 0) (value, Some("fizz")) else (value, None))

def buzz: StatefulPrint = ((value) => if ((value % 5) == 0) (value, Some("buzz")) else (value, None))

def combine(left: StatefulPrint, right: StatefulPrint)(f: (Option[String], Option[String]) => Option[String]) : StatefulPrint = ( value => {
  val theLeft = left(value)
  val theRight = right(value)

  (value, f(theLeft._2, theRight._2))
})



def concat(left: StatefulPrint, right: StatefulPrint) : StatefulPrint = combine(left, right)  {
  (a,b) => (a, b) match {
      case (Some(leftVal), Some(rightVal)) => Some(leftVal + rightVal)
      case (Some(leftVal), None) => Some(leftVal)
      case (None, Some(rightVal)) => Some(rightVal)
      case (None, None) => None
  }
}


def overwrite(left: StatefulPrint, right: StatefulPrint) : StatefulPrint = combine(left, right) {
  (a, b) => if(b.isDefined) b else a
}


def fizzBuzz: StatefulPrint = overwrite( id, concat(fizz, buzz))

def fizzBuzzString: Int => String = (fizzBuzz((_))._2.get)
Stream.from(1).map(fizzBuzzString(_)).take(30).toList





