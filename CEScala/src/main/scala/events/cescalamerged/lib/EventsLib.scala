package events.cescalamerged.lib

import com.espertech.esper.client._
import scala.collection.mutable
import scala.reflect._
import runtime.universe._
import scala.Array

object CEPEngine {

  import com.espertech.esper.client.Configuration

  // track different property type combinations
  private val propertiesToEsperEventMap = mutable.HashMap[Array[AnyRef], String]()

  // track which CEScala event corresponds to which Esper event
  protected[lib] val cescalaToEsperEventMap = mutable.HashMap[Int, String]()

  /* Configure the CEP engine */
  private val epConfig = new Configuration()
  epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
  val epService = EPServiceProviderManager.getDefaultProvider(epConfig)

  /* add Event Type with one or multiple properties which are named P1, P2, P3, etc. */
  def addEventType(name: Int, propertyTypes: Array[AnyRef]) {
    propertiesToEsperEventMap.get(propertyTypes) match {
      case Some(_) => // do nothing
      case _ =>
        val esperId = "C" + EsperIds.newId()
        propertiesToEsperEventMap.put(propertyTypes, esperId)
        cescalaToEsperEventMap.put(name, esperId)
        val propertyNames = (for (i <- 0 to propertyTypes.length) yield "P" + i).toArray
        epService.getEPAdministrator.getConfiguration.addEventType(esperId, propertyNames, propertyTypes.+:(classOf[Integer]))
    }
  }

  /* convert the properties of the event to an array
   * and send the event to the engine
   */
  def sendEvent[T: TypeTag](name: Integer, properties: T) {
    val event: Array[AnyRef] = typeOf[T] match {
      case t if t <:< typeOf[Product] =>
        properties.asInstanceOf[Product].productIterator.toArray.asInstanceOf[Array[AnyRef]]
      case _ =>
        Array(properties.asInstanceOf[AnyRef])
    }
    epService.getEPRuntime.sendEvent(event.+:(name), cescalaToEsperEventMap.get(name).get)
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
  def name: Int

  /**
   * Register a reaction to the event
   */
  def +=(react: T => Unit)

  /**
   * Unregister a reaction
   */
  def -=(react: T => Unit)

  /**
   * Unregister all reactions
   */
  def removeAllReactions()
}

/*
 *  Base class for events.
 *  Maintains reactions.
 */
abstract class EventNode[T] extends Event[T] {
  /*
   * Unique event identifier
   */
  val name = EventIds.newId()

  protected val listeners = mutable.Map[T => Unit, UpdateListener]()
  protected val stickyListeners = mutable.Map[T => Unit, UpdateListener]() // Listeners for mapped events must always stay

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
      event.getEventType.getPropertyNames.length match {
        case p if p == 0 || p == 1 => react(Unit.asInstanceOf[T])
        case 2 => react(event.get("P1").asInstanceOf[T])
        case p =>
          val properties = for (i <- 1 to p-1) yield event.get("P" + i)
          react(CEPEngine.toTuple(properties.toArray).asInstanceOf[T])
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

  protected val statement = CEPEngine.createEPL("select istream * from " + CEPEngine.cescalaToEsperEventMap.get(name).get + "(P0 = " + name + ")")

  /* Triggers the event */
  def apply(value: T) {
    CEPEngine.sendEvent(name, value)
  }

  override def toString = "ImperativeEvent: " + name
}

/*
 * Implicit type conversions
 */
object EventsLibConversions {
  // some implicit conversion for methods which allow to write
  // instrumented methods in a more intuitive way.
  implicit def toUnitfun[T](f: () => T) = (_: Unit) => f()

  implicit def toTupledFun2[T1, T2, R](f: (T1, T2) => R) = f.tupled

  implicit def toTupledFun3[T1, T2, T3, R](f: (T1, T2, T3) => R) = f.tupled

  implicit def toTupledFun4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R) = f.tupled

  implicit def toTupledFun5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = f.tupled

  implicit def toTupledFun6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R) = f.tupled

  implicit def toTupledFun7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R) = f.tupled

  implicit def toTupledFun8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = f.tupled

  implicit def toTupledFun9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = f.tupled

  implicit def toTupledFun10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = f.tupled
}

/* Fresh event ID generator */
class Ids {
  private var lastGeneratedId: Int = 0

  def newId(): Int = {
    lastGeneratedId += 1
    lastGeneratedId
  }
}

object EventIds extends Ids {}

object EsperIds extends Ids {}