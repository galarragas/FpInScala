package fpinscala.errorhandling

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


  //  def flatMap[B](f: A => Option[B]): Option[B] = ???
  //
  //  /*
  //  Of course, we can also implement `flatMap` with explicit pattern matching.
  //  */
  //  def flatMap_1[B](f: A => Option[B]): Option[B] = ???
  //
  //  def orElse[B>:A](ob: => Option[B]): Option[B] = ???
  //  /*
  //  Again, we can implement this with explicit pattern matching.
  //  */
  //  def orElse_1[B>:A](ob: => Option[B]): Option[B] = ???
  //
  //  def filter(f: A => Boolean): Option[A] = ???
  //  /*
  //  This can also be defined in terms of `flatMap`.
  //  */
  //  def filter_1(f: A => Boolean): Option[A] = ???



case class Client(name: String, address: Option[String])


trait WithAddress {
  def address = Some("Casa")
}

trait WithNoAddress {
  def address = None
}

trait WithClient {
  def address: Option[String]

  val client = Some(Client("Stefano", address))
}

trait WithoutClient {
  val client = None
}

class FlatMapTest extends FlatSpec with ShouldMatchers {
  val getAddress: (Client => Option[String]) = _.address

  "FlatMap" should "return SOME function result if applied to SOME" in {
    new WithClient with WithAddress {
      client.flatMap(getAddress) should equal(Some("Casa"))
    }
  }

  it should "return NONE function result if applied to SOME" in {
    new WithClient with WithNoAddress {
      client.flatMap(getAddress) should equal(None)
    }
  }

  it should "return NONE if applied to NONE" in {
    new WithoutClient {
      client.flatMap(getAddress) should equal(None)
    }
  }
}

class OrElseTest extends FlatSpec with ShouldMatchers {
  "OrElse" should "return ELSE if applied to NONE" in {
    new WithNoAddress {
      address.orElse(Some("else")) should equal(Some("else"))
    }
  }

  it should "return itself if applied to SOME" in {
    new WithAddress  {
      address.orElse(Some("else")) should equal(address)
    }
  }
}

//  def filter(f: A => Boolean): Option[A] = ???
class FilterTest extends FlatSpec with ShouldMatchers {
  def startWithC = {a: String => a.toLowerCase.startsWith("c")}
  def startWithS = {a: String => a.toLowerCase.startsWith("s")}

  "Filter" should "return itself if predicate is true" in {
    new WithAddress {
      address.filter(startWithC) should equal(address)
    }
  }

  it should "return NONE if predicate is False" in {
    new WithAddress {
      address.filter(startWithS) should equal(None)
    }
  }

  it should "return NONE if applied to None" in {
    new WithNoAddress  {
      address.filter(startWithC) should equal(None)
    }
  }
}

class Filter_1Test extends FlatSpec with ShouldMatchers {
  def startWithC = {a: String => a.toLowerCase.startsWith("c")}
  def startWithS = {a: String => a.toLowerCase.startsWith("s")}

  "Filter_1" should "return itself if predicate is true" in {
    new WithAddress {
      address.filter_1(startWithC) should equal(address)
    }
  }

  it should "return NONE if predicate is False" in {
    new WithAddress {
      address.filter_1(startWithS) should equal(None)
    }
  }

  it should "return NONE if applied to None" in {
    new WithNoAddress  {
      address.filter_1(startWithC) should equal(None)
    }
  }
}