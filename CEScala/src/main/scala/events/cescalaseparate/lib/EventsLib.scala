package events.cescalaseparate.lib

import scala.language.implicitConversions
import scala.language.postfixOps
import com.espertech.esper.client._
import scala.collection.mutable
import scala.reflect._
import runtime.universe._
import scala.{Product, Array}
import shapeless._, ops.tuple._, nat._
import com.espertech.esper.event.arr.ObjectArrayEventBean

object CEPEngine {
  /* Configure the CEP engine */
  val epConfig = new Configuration()
  epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
  val epService = EPServiceProviderManager.getDefaultProvider(epConfig)

  /* add Event Type with one or multiple properties which are named P1, P2, P3, etc. */
  def addEventType(name: String, propertyTypes: Array[AnyRef]) {
    val propertyNames = (for (i <- 1 to propertyTypes.length) yield "P" + i).toArray
    epService.getEPAdministrator.getConfiguration.addEventType(name, propertyNames, propertyTypes)
  }

  def removeEventType(name: String) {
    epService.getEPAdministrator.getConfiguration.removeEventType(name, false)
  }

  /* convert the properties of the event to an array
   * and send the event to the engine
   */
  def sendEvent[T: TypeTag](name: String, properties: T) {
    val event: Array[AnyRef] = typeOf[T] match {
      case t if t <:< typeOf[Product] =>
        properties.asInstanceOf[Product].productIterator.toArray.asInstanceOf[Array[AnyRef]]
      case _ =>
        Array(properties.asInstanceOf[AnyRef])
    }
    epService.getEPRuntime.sendEvent(event, name)
  }

  /* an interface to query the engine directly */
  def createEPL(s: String) = {
    CEPEngine.epService.getEPAdministrator.createEPL(s)
  }

  def toTuple[T <: Object](array: Array[T]) = {
    array.size match {
      case 1 => Tuple1(array(0))
      case 2 => Tuple2(array(0), array(1))
      case 3 => Tuple3(array(0), array(1), array(2))
      case 4 => Tuple4(array(0), array(1), array(2), array(3))
      case 5 => Tuple5(array(0), array(1), array(2), array(3), array(4))
      case 6 => Tuple6(array(0), array(1), array(2), array(3), array(4), array(5))
      case 7 => Tuple7(array(0), array(1), array(2), array(3), array(4), array(5), array(6))
      case 8 => Tuple8(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7))
      case 9 => Tuple9(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8))
      case 10 => Tuple10(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9))
      case 11 => Tuple11(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10))
      case 12 => Tuple12(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11))
      case 13 => Tuple13(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12))
      case 14 => Tuple14(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13))
      case 15 => Tuple15(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14))
      case 16 => Tuple16(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15))
      case 17 => Tuple17(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16))
      case 18 => Tuple18(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17))
      case 19 => Tuple19(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18))
      case 20 => Tuple20(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19))
      case 21 => Tuple21(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19), array(20))
      case 22 => Tuple22(array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19), array(20), array(21))
    }
  }
}

trait Event[+T] {
  /**
   * Unique event identifier
   */
  def name: String

  /**
   * Register a reaction to the event
   */
  def +=(react: T => Unit)

  /**
   * Reactions which represent event mappings and similar
   * operations must be registered independently in order
   * to make it impossible for them to be removed (e. g.
   * via removeAllReactions() ).
   */
  protected[lib] def addStickyListener(react: T => Unit)

  /**
   * Unregister a reaction
   */
  def -=(react: T => Unit)

  /**
   * Unregister all reactions
   */
  def removeAllReactions()

  /**
   * Events disjunction.
   */
  def ||[S >: T : ClassTag : TypeTag, U <: S](other: Event[U]) = new EventNodeOr[S](this, other)

  /**
   * Event filtered with a predicate
   */
  def &&[U >: T : ClassTag : TypeTag](pred: U => Boolean) = new EventNodeFilter[U](this, pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&[U >: T : ClassTag : TypeTag](pred: => Boolean) = new EventNodeFilter[U](this, _ => pred)

  /**
   * Transform the event parameter
   */
  def map[U: ClassTag : TypeTag, S >: T : TypeTag](mapping: S => U) = new EventNodeMap[S, U](this, mapping)

  /**
   * Use the Repeat Event Pattern
   */
  def repeat[U >: T](times: Int) = new EventNodeRepeat[U](this, times)

  protected[lib] def mapToTuple[U >: T] = new EventNodeMapToTuple[U](this)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam[S >: T : TypeTag] = new EventNodeMap[S, Unit](this, (_: Any) => ())
}

/*
 *  Base class for events.
 */
trait EventNode[T] extends Event[T] with Reactions[T] {
  /*
   * Unique event identifier
   */
  val name = "E" + NewEventId()
}

/*
 * Mixin trait for events.
 * Maintains reactions.
 */
trait Reactions[T] {
  private val listeners = mutable.Map[T => Unit, UpdateListener]()
  private val stickyListeners = mutable.Map[T => Unit, UpdateListener]() // Listeners for mapped events must always stay

  protected def statement: EPStatement

  /*
   * Creates a new instance of UpdateListener. If the event has no properties, the reaction will
   * be called without any parameters. If the event has one property, the reaction will be called
   * with this property as parameter. If the event has more than one property, those properties
   * will be wrapped in a tuple and the reaction will be called with the tuple as parameter.
   */
  protected def MyListener(react: T => Unit) = new UpdateListener {
    override def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]) {
      val event = newEvents(0)
      event.getEventType.getPropertyNames match {
        case p if p.length == 0 => react(Unit.asInstanceOf[T])
        case p if p.length == 1 =>
          react(event.get("P1").asInstanceOf[T])
        case p =>
          val properties = for (propertyName <- p) yield event.get(propertyName)
          react(CEPEngine.toTuple(properties).asInstanceOf[T])
      }
    }
  }

  /*
   * Register a reaction to the event
   */
  def +=(react: T => Unit) {
    val myListener = MyListener(react)
    listeners += (react -> myListener)
    statement.addListener(myListener)
  }

  /*
   * Reactions which represent event mappings and similar
   * operations must be registered independently in order
   * to make it impossible for them to be removed (e. g.
   * via removeAllReactions() ).
   */
  protected[lib] def addStickyListener(react: T => Unit) {
    val myListener = MyListener(react)
    stickyListeners += (react -> myListener)
    statement.addListener(myListener)
  }

  /*
   * Unregister a reaction
   */
  def -=(react: T => Unit) {
    listeners.get(react).foreach(statement.removeListener)
    listeners -= react
  }

  /*
   * Unregister all reactions
   */
  def removeAllReactions() {
    statement.removeAllListeners()
    stickyListeners.foreach(l => statement.addListener(l._2))
    listeners.clear()
  }
}

/*
 * An implementation of an imperative event
 */
class ImperativeEvent[T: ClassTag : TypeTag] extends EventNode[T] {

  /* Obtain the type of this event by reflection and create an event in the engine for it */
  typeOf[T] match {
    case t if t <:< typeOf[Product] => // Type is a tuple
      val typeArgs = t match {
        case TypeRef(_, _, args) => args
      }
      val m = runtimeMirror(getClass.getClassLoader)
      val propertyTypes: Array[AnyRef] = typeArgs.map(t => m.runtimeClass(t.typeSymbol.asClass)).toArray
      CEPEngine.addEventType(name, propertyTypes)
    case _ => // Type is not a tuple
      val propertyTypes: Array[AnyRef] = Array(classTag[T].runtimeClass)
      CEPEngine.addEventType(name, propertyTypes)
  }

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  /* Triggers the event */
  def apply(value: T) {
    CEPEngine.sendEvent(name, value)
  }

  override def toString = "ImperativeEvent: " + name
}

/*
 * Implementation of event disjunction
 */
class EventNodeOr[T: ClassTag : TypeTag](ev1: Event[_ <: T], ev2: Event[_ <: T]) extends EventNode[T] {
  /* Obtain the type of this event by reflection and create an event in the engine for it */
  typeOf[T] match {
    case t if t <:< typeOf[Product] => // Type is a tuple
      val typeArgs = t match {
        case TypeRef(_, _, args) => args
      }
      val m = runtimeMirror(getClass.getClassLoader)
      val propertyTypes: Array[AnyRef] = typeArgs.map(t => m.runtimeClass(t.typeSymbol.asClass)).toArray
      CEPEngine.addEventType(name, propertyTypes)
    case _ => // Type is not a tuple
      val propertyTypes: Array[AnyRef] = Array(classTag[T].runtimeClass)
      CEPEngine.addEventType(name, propertyTypes)
  }

  CEPEngine.createEPL("insert istream into " + name + " select * from " + ev1.name)
  CEPEngine.createEPL("insert istream into " + name + " select * from " + ev2.name)

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  override def toString = "(" + ev1.name + " || " + ev2.name + ")"
}

/*
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T: ClassTag : TypeTag](ev: Event[T], f: T => Boolean) extends EventNode[T] {
  CEPEngine.createEPL("create schema " + name + "() copyfrom " + ev.name)
  private val filterEvent = (properties: T) => if (f(properties)) {
    CEPEngine.sendEvent(name, properties)
  }
  ev.asInstanceOf[EventNode[T]].addStickyListener(filterEvent)

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  override def toString = "(" + ev.name + " && <predicate>)"
}


/*
 * Implements transformation of event parameter
 */
class EventNodeMap[T: TypeTag, U: ClassTag : TypeTag](ev: Event[T], f: T => U) extends EventNode[U] {
  typeOf[U] match {
    case t if t <:< typeOf[Product] =>
      val typeArgs = t match {
        case TypeRef(_, _, args) => args
      }
      val m = runtimeMirror(getClass.getClassLoader)
      val propertyTypes: Array[AnyRef] = typeArgs.map(t => m.runtimeClass(t.typeSymbol.asClass)).toArray
      CEPEngine.addEventType(name, propertyTypes)
    case _ =>
      val propertyTypes: Array[AnyRef] = Array(classTag[U].runtimeClass)
      CEPEngine.addEventType(name, propertyTypes)
  }

  private val mapEvent = (properties: T) => CEPEngine.sendEvent(name, f(properties))
  ev.addStickyListener(mapEvent)

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  override def toString = "(" + ev.name + ".map(...))"
}

class EventNodeMapToTuple[T](ev: Event[T]) extends EventNode[Tuple1[T]] {
  override val name = ev.name

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  override def toString = "(" + ev.name + ".mapToTuple)"
}

/*
 * Implementation of event join
 * 
 */
class EventNodeJoin[T <: Product, U <: Product, Z](ev1: Event[T],
                                                   ev2: Event[U],
                                                   window1: String,
                                                   window2: String,
                                                   condition: BoolExpr)
  extends EventNode[Z] {

  private val ev1PropCount = CEPEngine.epService.getEPAdministrator.getConfiguration.getEventType(ev1.name).getPropertyNames.length
  private val ev2PropCount = CEPEngine.epService.getEPAdministrator.getConfiguration.getEventType(ev2.name).getPropertyNames.length

  private val select = mutable.Buffer[String]()
  for (i <- 1 to ev1PropCount) select += ev1.name + ".P" + i + " as " + "P" + i
  for (i <- 1 to ev2PropCount) select += ev2.name + ".P" + i + " as " + "P" + (ev1PropCount + i)

  CEPEngine.createEPL(
    "insert istream into " + name +
      " select " + select.mkString(", ") +
      " from " + ev1.name + ".win:" + window1 + ", " + ev2.name + ".win:" + window2 +
      " where " + condition.repr)

  override val statement = CEPEngine.createEPL("select istream * from " + name)

  /*
   * Counts the number of properties of both the first and the second event and
   * determines how to call the reaction accordingly. Compare with MyListener in
   * EventNode. Because there are two events in a join, there are also more
   * cases which we need to distinguish.
   */
  override def MyListener(react: Z => Unit) = new UpdateListener {
    override def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]) {
      val event = newEvents(0)
      ev1PropCount + ev2PropCount match {
        case 0 => react(Unit.asInstanceOf[Z])
        case 1 => react(event.get("P1").asInstanceOf[Z])
        case p =>
          val properties = for (p <- 1 to p) yield event.get("P" + p)
          react(CEPEngine.toTuple(properties.toArray).asInstanceOf[Z])
      }
    }
  }

  override def toString = "(" + ev1.name + " joinOn(" + condition + ") " + ev2.name + ")"
}

/*
 * Implementation of repeat every event pattern.
 * Repeat Events cannot be used in event expressions.
 */
class EventNodeRepeat[T](event: Event[T], times: Int) extends Reactions[Seq[T]] {
  private val eventNames = for (i <- 0 until times) yield "e[" + i + "]"
  val statement = CEPEngine.createEPL("select istream " + eventNames.mkString(",") + " from pattern[every [" + times + "] e=" + event.name + "]")

  override def MyListener(react: Seq[T] => Unit) = new UpdateListener {
    override def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]) {
      val eventProperties = for (i <- 0 until times) yield {
        val event = newEvents(0).asInstanceOf[ObjectArrayEventBean].get(eventNames(i)).asInstanceOf[Array[AnyRef]]
        event.size match {
          case 0 => Unit.asInstanceOf[T]
          case 1 => event(0).asInstanceOf[T]
          case _ => CEPEngine.toTuple(event).asInstanceOf[T]
        }
      }
      react(eventProperties)
    }
  }

  override def toString = "[" + times + "] " + event.name
}

trait DSLWindow {
  val repr: String
}

case class length(value: Int) extends DSLWindow {
  val repr = "length(" + value + ")"
}

case class time(value: DSLWindowTime) extends DSLWindow {
  val repr = "time(" + value.repr + ")"
}

trait DSLWindowTime extends DSLWindow

case class DSLWindowTimeBuilder(value: Int) {
  def msec = DSLWindowTimeMSec(value)

  def sec = DSLWindowTimeSec(value)

  def min = DSLWindowTimeMin(value)

  def hour = DSLWindowTimeHours(value)

  def hours = DSLWindowTimeHours(value)

  def day = DSLWindowTimeDays(value)

  def days = DSLWindowTimeDays(value)
}

case class DSLWindowTimeMSec(value: Int) extends DSLWindowTime {
  val repr = value + " msec"
}

case class DSLWindowTimeSec(value: Int) extends DSLWindowTime {
  val repr = value + " sec"
}

case class DSLWindowTimeMin(value: Int) extends DSLWindowTime {
  val repr = value + " min"
}

case class DSLWindowTimeHours(value: Int) extends DSLWindowTime {
  val repr = value + " hours"
}

case class DSLWindowTimeDays(value: Int) extends DSLWindowTime {
  val repr = value + " days"
}

case class DSLJoinWindow[T <: Product, U <: Product](ev1: Event[T], ev2: Event[U]) {
  /**
   * Define the window for both events
   */
  def window(window: DSLWindow) = DSLJoinWindowOn[T, U](this, window)
}

case class DSLJoinWindowOn[T <: Product, U <: Product](events: DSLJoinWindow[T, U], window: DSLWindow) {
  /**
   * Define the condition on which the join is performed
   */
  def on(condition: (Event[T], Event[U]) => BoolExpr)(implicit prepend: Prepend[T, U]) = new EventNodeJoin[T, U, prepend.Out](events.ev1, events.ev2, window.repr, window.repr, condition(events.ev1, events.ev2))
}

case class DSLWindowJoin[T <: Product](event: Event[T], window: DSLWindow) {
  /**
   * Join the Event with another Event
   */
  def join[U <: Product](other: DSLWindowJoin[U]) = new DSLWindowJoinOn[T, U](this, other)
}

case class DSLWindowJoinOn[T <: Product, U <: Product](ev1: DSLWindowJoin[T], ev2: DSLWindowJoin[U]) {
  /**
   * Define the condition on which the join is performed
   */
  def on(condition: (Event[T], Event[U]) => BoolExpr)(implicit prepend: Prepend[T, U]) = new EventNodeJoin[T, U, prepend.Out](ev1.event, ev2.event, ev1.window.repr, ev2.window.repr, condition(ev1.event, ev2.event))
}

case class TupleEvent[T <: Product](event: Event[T]) {
  /**
   * Add a window to the Event (e.g. last 30 seconds: "time(30 sec)")
   * Used for Event Joining
   */
  def join[U <: Product](other: TupleEvent[U]) = new DSLJoinWindow[T, U](event, other.event)

  /**
   * Add a window to the Event (e.g. last 30 seconds: "time(30 sec)")
   * Used for Event Joining
   */
  def window(window: DSLWindow) = new DSLWindowJoin[T](event, window)
}

trait BoolExpr {
  val repr: String

  def &&(other: BoolExpr) = BoolExprNode(repr + " AND " + other.repr)

  def ||(other: BoolExpr) = BoolExprNode(repr + " OR " + other.repr)
}

case class BoolExprNode(repr: String) extends BoolExpr

case class BoolNullExpr(repr: String) extends BoolExpr

case class \(expr: BoolExprNode) extends BoolExpr {
  val repr = "NOT " + expr.repr
}

case class ValueExpr[T](repr: String) {
  def ===(other: ValueExpr[T]) = BoolExprNode(repr + " = " + other.repr)

  def !==(other: ValueExpr[T]) = BoolExprNode(repr + " != " + other.repr)

  def <(other: ValueExpr[T]) = BoolExprNode(repr + " < " + other.repr)

  def >(other: ValueExpr[T]) = BoolExprNode(repr + " > " + other.repr)

  def <=(other: ValueExpr[T]) = BoolExprNode(repr + " <= " + other.repr)

  def >=(other: ValueExpr[T]) = BoolExprNode(repr + " >= " + other.repr)

  def isNull = BoolNullExpr(repr + " is null")

  def isNotNull = BoolNullExpr(repr + " is not null")
}

case class ValueEventExpr[T](repr: String) {
  def _1 = ValueExpr[T](repr + ".P1")
}

case class TupleEventExpr[T](repr: String) {
  def _1(implicit at: At[T, _0]) = ValueExpr[at.Out](repr + ".P1")

  def _2(implicit at: At[T, _1]) = ValueExpr[at.Out](repr + ".P2")

  def _3(implicit at: At[T, _2]) = ValueExpr[at.Out](repr + ".P3")

  def _4(implicit at: At[T, _3]) = ValueExpr[at.Out](repr + ".P4")

  def _5(implicit at: At[T, _4]) = ValueExpr[at.Out](repr + ".P5")

  def _6(implicit at: At[T, _5]) = ValueExpr[at.Out](repr + ".P6")

  def _7(implicit at: At[T, _6]) = ValueExpr[at.Out](repr + ".P7")

  def _8(implicit at: At[T, _7]) = ValueExpr[at.Out](repr + ".P8")

  def _9(implicit at: At[T, _8]) = ValueExpr[at.Out](repr + ".P9")

  def _10(implicit at: At[T, _9]) = ValueExpr[at.Out](repr + ".P10")

  def _11(implicit at: At[T, _10]) = ValueExpr[at.Out](repr + ".P11")

  def _12(implicit at: At[T, _11]) = ValueExpr[at.Out](repr + ".P12")

  def _13(implicit at: At[T, _12]) = ValueExpr[at.Out](repr + ".P13")

  def _14(implicit at: At[T, _13]) = ValueExpr[at.Out](repr + ".P14")

  def _15(implicit at: At[T, _14]) = ValueExpr[at.Out](repr + ".P15")

  def _16(implicit at: At[T, _15]) = ValueExpr[at.Out](repr + ".P16")

  def _17(implicit at: At[T, _16]) = ValueExpr[at.Out](repr + ".P17")

  def _18(implicit at: At[T, _17]) = ValueExpr[at.Out](repr + ".P18")

  def _19(implicit at: At[T, _18]) = ValueExpr[at.Out](repr + ".P19")

  def _20(implicit at: At[T, _19]) = ValueExpr[at.Out](repr + ".P20")

  def _21(implicit at: At[T, _20]) = ValueExpr[at.Out](repr + ".P21")

  def _22(implicit at: At[T, _21]) = ValueExpr[at.Out](repr + ".P22")
}

/*
 * Implicit type conversions
 */
object EventsLibConversions {
  implicit def dropParam[T: TypeTag](ev: Event[T]) = ev.dropParam

  implicit def toUnitfun[T](f: () => T) = (_: Unit) => f()

  implicit def toTuple1Event[T](event: Event[T]) = TupleEvent(event.mapToTuple)

  implicit def toTupleEvent[T <: Product](event: Event[T]) = TupleEvent(event)

  implicit def toDSLWindowTime(value: Int) = DSLWindowTimeBuilder(value)

  implicit def toValueEventExpr[T](event: Event[T]) = ValueEventExpr[T](event.name)

  implicit def toTupleEventExpr[T <: Product](event: Event[T]) = TupleEventExpr[T](event.name)

  implicit def toTupledFun2[T1, T2, R](f: (T1, T2) => R) = f.tupled

  implicit def toTupledFun3[T1, T2, T3, R](f: (T1, T2, T3) => R) = f.tupled

  implicit def toTupledFun4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R) = f.tupled

  implicit def toTupledFun5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = f.tupled

  implicit def toTupledFun6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R) = f.tupled

  implicit def toTupledFun7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R) = f.tupled

  implicit def toTupledFun8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = f.tupled

  implicit def toTupledFun9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = f.tupled

  implicit def toTupledFun10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = f.tupled

  implicit def toTupledFun11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R) = f.tupled

  implicit def toTupledFun12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R) = f.tupled

  implicit def toTupledFun13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R) = f.tupled

  implicit def toTupledFun14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R) = f.tupled

  implicit def toTupledFun15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R) = f.tupled

  implicit def toTupledFun16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R) = f.tupled

  implicit def toTupledFun17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R) = f.tupled

  implicit def toTupledFun18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R) = f.tupled

  implicit def toTupledFun19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R) = f.tupled

  implicit def toTupledFun20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R) = f.tupled

  implicit def toTupledFun21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R) = f.tupled

  implicit def toTupledFun22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R) = f.tupled
}

/* Fresh event ID generator */
object NewEventId {
  private var lastGeneratedId: Int = 0

  def apply(): Int = {
    lastGeneratedId += 1
    lastGeneratedId
  }
}