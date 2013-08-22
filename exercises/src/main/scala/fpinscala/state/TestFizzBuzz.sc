import com.pragmasoft.tests.fizzbuzz._

import com.pragmasoft.tests.fizzbuzz.NumberPrinter._

def fizz: NumberPrinter = NumberPrinter((value) => if ((value % 3) == 0) Some("fizz") else None)
def buzz: NumberPrinter = NumberPrinter((value) => if ((value % 5) == 0) Some("buzz") else None)
def fizzBuzz: NumberPrinter =  id overwrite (fizz append buzz)
def fizzBuzzString: Int => String = fizzBuzz(_).get
Stream.from(1).map(fizzBuzzString(_)).take(30).toList










