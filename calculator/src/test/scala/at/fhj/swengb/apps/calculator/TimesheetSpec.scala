package at.fhj.swengb.apps.calculator

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.util
import java.util.List
import java.nio.file.Files.readAllLines
import java.nio.file.Paths.get

import scala.collection.JavaConverters._
import org.scalatest.WordSpecLike

class TimesheetSpec extends WordSpecLike {

  private val filePath: Path = Paths.get("C:/workspace/fhj.swengb2017.assignments/calculator/timesheet-calculator.adoc")

  private val defaultText =
    """== Time expenditure: Calculator assignment
        |
        |[cols="1,1,4", options="header"]
        |.Time expenditure
        ||===
        || Date
        || Hours
        || Description
        |
        || 29.11.17
        || 1
        || Review of this and that
        |
        || 30.11.17
        || 5
        || implemented css
        |
        || 11.07.17
        || 2
        || fixed bugs
        |
        ||===""".stripMargin


  "not default Text" in {
        val scanfile:
          Seq[String] = readAllLines(filePath).asScala
        assert(scanfile.mkString("\n")!= defaultText)
      }

}
