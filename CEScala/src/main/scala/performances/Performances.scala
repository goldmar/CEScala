package performances

import scala.Predef._
import events.escala.lib.{ImperativeEvent => EScalaEvent}
import events.cescalaseparate.lib.{ImperativeEvent => CEScalaSeparateEvent}
import events.cescalamerged.lib.{ImperativeEvent => CEScalaMergedEvent}
import com.espertech.esper.client.{EventBean, UpdateListener, EPServiceProviderManager, Configuration}
import org.apache.log4j.{Level, Logger, SimpleLayout, ConsoleAppender}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.FileWriter
import scala.math.BigDecimal.RoundingMode

object PerformanceTest extends App {
  val name = args.head
  name match {
    case "SP.EScala" => Measurer.createMeasurement(name, CEPTests.escalaSinglePropertyPerformanceTest)
    case "SP.EsperListener" => Measurer.createMeasurement(name, CEPTests.esperListenerSinglePropertyPerformanceTest)
    case "SP.EsperSubscriber" => Measurer.createMeasurement(name, CEPTests.esperSubscriberSinglePropertyPerformanceTest)
    case "SP.CEScalaSeparate" => Measurer.createMeasurement(name, CEPTests.cescalaSeparateSinglePropertyPerformanceTest)
    case "SP.CEScalaMerged" => Measurer.createMeasurement(name, CEPTests.cescalaMergedSinglePropertyPerformanceTest)
    case "TP.EScala" => Measurer.createMeasurement(name, CEPTests.escalaTwoPropertiesPerformanceTest)
    case "TP.EsperListener" => Measurer.createMeasurement(name, CEPTests.esperListenerTwoPropertiesPerformanceTest)
    case "TP.EsperSubscriber" => Measurer.createMeasurement(name, CEPTests.esperSubscriberTwoPropertiesPerformanceTest)
    case "TP.CEScalaSeparate" => Measurer.createMeasurement(name, CEPTests.cescalaSeparateTwoPropertiesPerformanceTest)
    case "TP.CEScalaMerged" => Measurer.createMeasurement(name, CEPTests.cescalaMergedTwoPropertiesPerformanceTest)
    case _ => println("Unknown argument.")
  }
}

object Measurer {
  val events = List(100, 200, 400, 800)
  val execTimes = 100

  val measurements = new ListBuffer[Long]

  def createMeasurement(name: String, method: (Int, Int) => Long) {
    method(events.last * 5, execTimes * 5) // warmup
    System.gc()                            // running gc

    for (i <- events) {
      System.gc()                          // running gc
      val start = System.nanoTime
      method(i, execTimes)
      val end = System.nanoTime
      measurements += (end - start)
    }

    val fileName = name + ".csv"
    val lines = try {
      Source.fromFile(fileName).getLines().toList
    } catch {
      case _: Throwable => for (i <- 1 to 4) yield ""
    }
    val fw = new FileWriter(fileName, false)
    try {
      for (i <- 0 until events.size) {
        val measurement = BigDecimal(measurements(i)) / 1000000 setScale(3, RoundingMode.HALF_UP) toString()
        lines(i) match {
          case "" => fw.write(measurement + "\n")
          case _ => fw.write(lines(i) + " " + measurement + "\n")
        }
      }
    }
    finally fw.close()
  }
}

object CEPTests {
  /* General Performance Test
   * What is the performance of triggering (events) events (execTimes) amount of times?
   *
   * Multiple Properties Performance Test
   * What is the performance of triggering (events) events with two properties (execTimes) amount of times? */


  val appender = new ConsoleAppender(new SimpleLayout())
  Logger.getRootLogger.addAppender(appender)
  Logger.getRootLogger.setLevel(Level.WARN)

  def escalaSinglePropertyPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: Long) => test += e
    val escalaEvents = for (_ <- 1 to events) yield new EScalaEvent[Long]
    escalaEvents.foreach(e => e += r)
    repeat(execTimes) {
      escalaEvents.foreach(e => e(1))
    }
    test
  }

  def cescalaSeparateSinglePropertyPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: Long) => test += e
    val cescala1Events = for (_ <- 1 to events) yield new CEScalaSeparateEvent[Long]
    cescala1Events.foreach(e => e += r)
    repeat(execTimes) {
      cescala1Events.foreach(e => e(1))
    }
    test
  }

  def cescalaMergedSinglePropertyPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: Long) => test += e
    val cescala2Events = for (_ <- 1 to events) yield new CEScalaMergedEvent[Long]
    cescala2Events.foreach(e => e += r)
    repeat(execTimes) {
      cescala2Events.foreach(e => e(1))
    }
    test
  }


  def esperSubscriberSinglePropertyPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val epConfig = new Configuration()
    epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
    val epService = EPServiceProviderManager.getDefaultProvider(epConfig)
    val p1 = Array("P1")
    val long: Array[AnyRef] = Array(classOf[java.lang.Long])
    object subscriber {
      def update(P1: Long) {
        test += P1
      }
    }
    val esperEvents = for (i <- 1 to events) yield {
      val name = "Esper" + events + "S" + i
      epService.getEPAdministrator.getConfiguration.addEventType(name, p1, long)
      val statement = epService.getEPAdministrator.createEPL("select istream P1 from " + name)
      statement.setSubscriber(subscriber)
      name
    }
    val esperEvent = Array(1L.asInstanceOf[AnyRef])
    repeat(execTimes) {
      esperEvents.foreach(e => epService.getEPRuntime.sendEvent(esperEvent, e))
    }
    test
  }

  def esperListenerSinglePropertyPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val epConfig = new Configuration()
    epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
    val epService = EPServiceProviderManager.getDefaultProvider(epConfig)
    val p1 = Array("P1")
    val long: Array[AnyRef] = Array(classOf[java.lang.Long])
    val listener = new UpdateListener {
      override def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]) {
        test += newEvents(0).get("P1").asInstanceOf[Long]
      }
    }
    val esperEvents = for (i <- 1 to events) yield {
      val name = "Esper" + events + "L" + i
      epService.getEPAdministrator.getConfiguration.addEventType(name, p1, long)
      val statement = epService.getEPAdministrator.createEPL("select istream P1 from " + name)
      statement.addListener(listener)
      name
    }
    val esperEvent = Array(1L.asInstanceOf[AnyRef])
    repeat(execTimes) {
      esperEvents.foreach(e => epService.getEPRuntime.sendEvent(esperEvent, e))
    }
    test
  }

  def escalaTwoPropertiesPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: (Long, Long)) => test += e._1 + e._2
    val escalaEvents = for (_ <- 1 to events) yield new EScalaEvent[(Long, Long)]
    escalaEvents.foreach(e => e += r)
    repeat(execTimes) {
      escalaEvents.foreach(e => e((1, 1)))
    }
    test
  }

  def cescalaSeparateTwoPropertiesPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: (Long, Long)) => test += e._1 + e._2
    val cescala1Events = for (_ <- 1 to events) yield new CEScalaSeparateEvent[(Long, Long)]
    cescala1Events.foreach(e => e += r)
    repeat(execTimes) {
      cescala1Events.foreach(e => e((1, 1)))
    }
    test
  }

  def cescalaMergedTwoPropertiesPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val r = (e: (Long, Long)) => test += e._1 + e._2
    val cescala2Events = for (_ <- 1 to events) yield new CEScalaMergedEvent[(Long, Long)]
    cescala2Events.foreach(e => e += r)
    repeat(execTimes) {
      cescala2Events.foreach(e => e((1, 1)))
    }
    test
  }

  def esperSubscriberTwoPropertiesPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val epConfig = new Configuration()
    epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
    val epService = EPServiceProviderManager.getDefaultProvider(epConfig)
    val esperEvents1 = for (i <- 1 to events) yield {
      val p1 = Array("P1", "P2")
      val long: Array[AnyRef] = Array(classOf[java.lang.Long], classOf[java.lang.Long])
      epService.getEPAdministrator.getConfiguration.addEventType("Esper" + events + "MS" + i, p1, long)
      "Esper" + events + "MS" + i
    }
    object subscriber {
      def update(P1: Long, P2: Long) {
        test += P1 + P2
      }
    }
    for (i <- 1 to events) {
      val statement1 = epService.getEPAdministrator.createEPL("select istream P1, P2 from Esper" + events + "MS" + i)
      statement1.setSubscriber(subscriber)
    }
    val esperEvent = Array(1L.asInstanceOf[AnyRef], 1L.asInstanceOf[AnyRef])
    repeat(execTimes) {
      esperEvents1.foreach(e => epService.getEPRuntime.sendEvent(esperEvent, e))
    }
    test
  }

  def esperListenerTwoPropertiesPerformanceTest(events: Int, execTimes: Int) = {
    var test = 0L
    val epConfig = new Configuration()
    epConfig.getEngineDefaults.getEventMeta.setDefaultEventRepresentation(Configuration.EventRepresentation.OBJECTARRAY)
    val epService = EPServiceProviderManager.getDefaultProvider(epConfig)
    val esperEvents2 = for (i <- 1 to events) yield {
      val p1 = Array("P1", "P2")
      val long: Array[AnyRef] = Array(classOf[java.lang.Long], classOf[java.lang.Long])
      epService.getEPAdministrator.getConfiguration.addEventType("Esper" + events + "ML" + i, p1, long)
      "Esper" + events + "ML" + i
    }
    val listener = new UpdateListener {
      override def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]) {
        test += newEvents(0).get("P1").asInstanceOf[Long] + newEvents(0).get("P2").asInstanceOf[Long]
      }
    }
    for (i <- 1 to events) {
      val statement2 = epService.getEPAdministrator.createEPL("select istream P1, P2 from Esper" + events + "ML" + i)
      statement2.addListener(listener)
    }
    val esperEvent = Array(1L.asInstanceOf[AnyRef], 1L.asInstanceOf[AnyRef])
    repeat(execTimes) {
      esperEvents2.foreach(e => epService.getEPRuntime.sendEvent(esperEvent, e))
    }
    test
  }

  def repeat[R](n: Int)(block: => R) {
    for (_ <- 1 to n) {
      block
    }
  }
}