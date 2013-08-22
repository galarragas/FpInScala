package com.pragmasoft.tests.fizzbuzz {
  object ComposableFunction {
    def apply[A, B](f: A => B) = new ComposableFunction[A, B] {
      def apply(value: A): B = f(value)
    }
  }


  trait ComposableFunction[A, B] extends Function[A, B] {
    def combine[C, D](that: ComposableFunction[A, C])(f: (B, C) => D): ComposableFunction[A, D] =
      ComposableFunction((value) => f(this.apply(value), that.apply(value)))

  }

  object NumberPrinter {
    def id = NumberPrinter(value => Some(value.toString))

    def apply(f: Int => Option[String]) = new NumberPrinter {
      def apply(value: Int): Option[String] = f(value)
    }

  }

  trait NumberPrinter extends Function[Int, Option[String]] {
    def combine(that : NumberPrinter)(f : (Option[String], Option[String]) => Option[String]) =
      NumberPrinter((value : Int) => f(this(value), that(value)))

    def append(that: NumberPrinter) : NumberPrinter = combine(that)  {
      (a,b) => a.flatMap( {
        left => Some( left + {
          b match {
            case Some(right) => right
            case _ => ""
          }
        }
        )
      }).orElse(b)
    }

    def overwrite(that: NumberPrinter) : NumberPrinter = combine(that) {
      (a, b) => if(b.isDefined) b else a
    }
  }
}