package events.lib

import events.cescalaseparate.lib._
import events.cescalaseparate.lib.EventsLibConversions._
import events.cescalamerged.lib.{ImperativeEvent => CEScalaMergedEvent}
import events.escala.lib.{ImperativeEvent => EScalaEvent}
import org.scalatest.{time => _, _}
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.Predef._
import org.apache.log4j.{ConsoleAppender, SimpleLayout, Level, Logger}

class EventsSpec extends FlatSpec with BeforeAndAfter {
  val appender = new ConsoleAppender(new SimpleLayout())
  Logger.getRootLogger.addAppender(appender)
  Logger.getRootLogger.setLevel(Level.WARN)

  var test = 0

  before {
    test = 0
  }

  "An Imperative Event with a single property" should "trigger a reaction when it's attached" in {
    val e1 = new ImperativeEvent[Int]
    val r1 = (e: Int) => test += e
    e1 += r1
    e1(1)
    assert(test === 1)
  }

  "An Imperative Event with a single property" should "not do anything when there is no reaction attached" in {
    val e1 = new ImperativeEvent[Int]
    val r1 = (e: Int) => test += e
    e1 += r1
    e1 -= r1
    e1(1)
    assert(test === 0)
  }

  "An Imperative Event with multiple properties" should "trigger a reaction when it's attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e1 += r1
    e1(1, "12345")
    e1(2, "67890")
    assert(test === 13)
  }

  "An Imperative Event with multiple properties" should "not do anything when there is no reaction attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e1 += r1
    e1 -= r1
    e1(1, "12345")
    e1(2, "67890")
    assert(test === 0)
  }

  "An Event Transformation with a single property" should "trigger a reaction when parameters are dropped and it's attached" in {
    val e1 = new ImperativeEvent[Int]
    val e2 = e1.dropParam
    val r1 = () => test += 1
    e2 += r1
    e1(1)
    e1(2)
    e1(3)
    assert(test === 3)
  }

  "An Event Transformation with a single property" should "trigger a reaction when it's attached" in {
    val e1 = new ImperativeEvent[String]
    val e2 = e1.map((e: String) => e.length * 2)
    val r1 = (e: Int) => test += e
    e2 += r1
    e1("1")
    e1("12")
    e1("123")
    assert(test === 12)
  }

  "An Event Transformation with a single property" should "not do anything when it's not attached" in {
    val e1 = new ImperativeEvent[String]
    val e2 = e1.map((e: String) => e.length * 2)
    val r1 = (e: Int) => test += e
    e2 += r1
    e2 -= r1
    e1("1")
    e1("12")
    e1("123")
    assert(test === 0)
  }

  "An Event Transformation with multiple properties" should "trigger a reaction when parameters are dropped and it's attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = e1.dropParam
    val r1 = () => test += 1
    e2 += r1
    e1(1, "test")
    e1(2, "test")
    e1(3, "test")
    assert(test === 3)
  }

  "An Event Transformation with multiple properties" should "trigger a reaction when it's attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = e1.map((e: (Int, String)) => (e._1, e._2.length))
    val r1 = (e: (Int, Int)) => test += e._1 + e._2
    e2 += r1
    e1(1, "abc")
    e1(2, "de")
    e1(3, "f")
    assert(test === 12)
  }

  "An Event Transformation with multiple properties" should "should not do anything when it's not attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = e1.map((e: (Int, String)) => (e._1, e._2.length))
    val r1 = (e: (Int, Int)) => test += e._1 + e._2
    e2 += r1
    e2 -= r1
    e1(1, "abc")
    e1(2, "de")
    e1(3, "f")
    assert(test === 0)
  }

  "An Event Disjunction with a single property" should "trigger a reaction on both events when it's attached" in {
    val e1 = new ImperativeEvent[Int]
    val e2 = new ImperativeEvent[Int]
    val e3 = e1 || e2
    val r1 = (e: Int) => test += e
    e3 += r1
    e1(1)
    e2(2)
    assert(test === 3)
  }

  "An Event Disjunction with a single property" should "not do anything on both events when it's not attached" in {
    val e1 = new ImperativeEvent[Int]
    val e2 = new ImperativeEvent[Int]
    val e3 = e1 || e2
    val r1 = (e: Int) => test += e
    e3 += r1
    e3 -= r1
    e1(1)
    e2(2)
    assert(test === 0)
  }

  "An Event Disjunction with multiple properties" should "trigger a reaction on both events when it's attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = new ImperativeEvent[(Int, String)]
    val e3 = e1 || e2
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e3 += r1
    e1(1, "12345")
    e2(2, "67890")
    assert(test === 13)
  }

  "An Event Disjunction with multiple properties" should "not do anything on both events when it's not attached" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = new ImperativeEvent[(Int, String)]
    val e3 = e1 || e2
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e3 += r1
    e3 -= r1
    e1(1, "12345")
    e2(2, "67890")
    assert(test === 0)
  }

  "An EScala Event Disjunction" should "select only one occurrance when two events match the same occurrance" in {
    val e1 = new EScalaEvent[Int]
    val e2 = e1 && true
    val e3 = e1 && true
    val e4 = e2 || e3
    val r1 = (e: Int) => test += e
    e4 += r1
    e1(1)
    e1(2)
    assert(test === 3)
  }

  "A CEScala Event Disjunction" should "select only both occurrances when two events match the same occurrance" in {
    val e1 = new ImperativeEvent[Int]
    val e2 = e1 && true
    val e3 = e1 && true
    val e4 = e2 || e3
    val r1 = (e: Int) => test += e
    e4 += r1
    e1(1)
    e1(2)
    assert(test === 6)
  }

  "An Event with multiple properties Filtered by a predicate" should "only trigger a reaction when the condition is true" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val predicate = (int: Int, string: String) => int < 10
    val e2 = e1 && predicate
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e2 += r1
    e1(1, "HIT")
    e1(2, "HIT")
    e1(11, "NOT")
    e1(12, "NOT")
    assert(test === 9)
  }

  "An Event with a single property Filtered by a boolean variable" should "only trigger a reaction when the condition is true" in {
    val e1 = new ImperativeEvent[Int]
    val e2 = e1 && true
    val e3 = e1 && false
    val r1 = (e: Int) => test += e
    val r2 = (e: Int) => test += 10 * e
    e2 += r1
    e3 += r2
    e1(1)
    e1(2)
    assert(test === 3)
  }

  "An Event Join" should "trigger a reaction when two events which are joined are received and the condition matches" in {
    import EventsLibConversions._
    var testString = ""
    val e1 = new ImperativeEvent[Int]
    val e2 = new ImperativeEvent[(Int, String)]
    val e3 = e1.window (time(30 sec)) join e2.window(time(30 sec)) on ((a,b) => a._1 === b._1)
    val r1 = (e: (Int, Int, String)) => testString += e._3
    e3 += r1
    e1(1)
    e1(2)
    e1(4)
    e1(5)
    e1(8)
    e2(1, "This ")
    e2(2, "is ")
    e2(3, "gibberish")
    e2(4, "some ")
    e2(5, "secret ")
    e2(6, "gibberish ")
    e2(7, "gibberish ")
    e2(8, "message.")
    assert(testString === "This is some secret message.")
  }

  "An Event Join with simplified syntax" should "trigger a reaction when two events which are joined are received and the condition matches" in {
    import EventsLibConversions._
    var testString = ""
    val e1 = new ImperativeEvent[Int]
    val e2 = new ImperativeEvent[(Int, String)]
    val e3 = e1 join e2 window time(30 sec) on ((a,b) => a._1 === b._1)
    val r1 = (e: (Int, Int, String)) => testString += e._3
    e3 += r1
    e1(1)
    e1(2)
    e1(4)
    e1(5)
    e1(8)
    e2(1, "This ")
    e2(2, "is ")
    e2(3, "gibberish")
    e2(4, "some ")
    e2(5, "secret ")
    e2(6, "gibberish ")
    e2(7, "gibberish ")
    e2(8, "message.")
    assert(testString === "This is some secret message.")
  }

  "A Repeat Event" should "execute the reactions the correct number of times" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = e1 repeat 3
    val r1 = (e: Seq[(Int, String)]) => test += 1
    e2 += r1
    e1(1, "one")
    e1(2, "two")
    e1(3, "three")
    e1(4, "four")
    e1(5, "five")
    e1(6, "six")
    e1(7, "seven")
    e1(8, "eight")
    e1(9, "nine")
    assert(test === 3)
  }

  "A Repeat Event" should "reference the correct properties in its listener" in {
    val e1 = new ImperativeEvent[(Int, String)]
    val e2 = e1 repeat 3
    val r1 = (e: Seq[(Int, String)]) => test += e(2)._1
    e2 += r1
    e1(1, "one")
    e1(2, "two")
    e1(3, "three")
    e1(4, "four")
    e1(5, "five")
    e1(6, "six")
    e1(7, "seven")
    e1(8, "eight")
    e1(9, "nine")
    assert(test === 18)
  }

  "An Event Join using a custom class with simplified syntax" should "trigger a reaction when two events which are joined are received and the condition matches" in {
    import EventsLibConversions._
    var testString = ""
    val e1 = new ImperativeEvent[IntString]
    val e2 = new ImperativeEvent[IntString]
    val e3 = e1 join e2 window time(30 sec) on ((a,b) => a._1 === b._1)
    val r1 = (e: (IntString, IntString)) => testString += e._2.string
    e3 += r1
    e1(new IntString(1, "one"))
    e1(new IntString(2, "two"))
    e1(new IntString(4, "four"))
    e1(new IntString(5, "five"))
    e1(new IntString(8, "eight"))
    e2(new IntString(1, "This "))
    e2(new IntString(2, "is "))
    e2(new IntString(3, "gibberish"))
    e2(new IntString(4, "some "))
    e2(new IntString(5, "secret "))
    e2(new IntString(7, "gibberish "))
    e2(new IntString(6, "gibberish "))
    e2(new IntString(8, "message."))
    assert(testString === "This is some secret message.")
  }

  "A CEScalaMerged Imperative Event with a single property" should "trigger a reaction when it's attached" in {
    val e1 = new CEScalaMergedEvent[Int]
    val r1 = (e: Int) => test += e
    e1 += r1
    e1(1)
    assert(test === 1)
  }

  "A CEScalaMerged Imperative Event with a single property" should "not do anything when there is no reaction attached" in {
    val e1 = new CEScalaMergedEvent[Int]
    val r1 = (e: Int) => test += e
    e1 += r1
    e1 -= r1
    e1(1)
    assert(test === 0)
  }

  "A CEScalaMerged Imperative Event with multiple properties" should "trigger a reaction when it's attached" in {
    val e1 = new CEScalaMergedEvent[(Int, String)]
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e1 += r1
    e1(1, "12345")
    e1(2, "67890")
    assert(test === 13)
  }

  "A CEScalaMerged Imperative Event with multiple properties" should "not do anything when there is no reaction attached" in {
    val e1 = new CEScalaMergedEvent[(Int, String)]
    val r1 = (e: (Int, String)) => test += e._1 + e._2.length
    e1 += r1
    e1 -= r1
    e1(1, "12345")
    e1(2, "67890")
    assert(test === 0)
  }
}

class IntString(val int: Int, val string: String) {
  override def equals(o: Any) = o match {
    case o: IntString => int == o.int
    case _ => false
  }
  override def hashCode = int
}