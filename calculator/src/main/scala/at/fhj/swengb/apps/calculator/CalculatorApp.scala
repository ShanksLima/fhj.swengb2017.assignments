package at.fhj.swengb.apps.calculator

import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.TextField
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

object CalculatorApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[CalculatorFX], args: _*)
  }
}

class CalculatorFX extends javafx.application.Application {

  val fxml = "/at/fhj/swengb/apps/calculator/calculator.fxml"
  val css = "/at/fhj/swengb/apps/calculator/calculator.css"

  def mkFxmlLoader(fxml: String): FXMLLoader = {
    new FXMLLoader(getClass.getResource(fxml))
  }

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("Calculator")
      setSkin(stage, fxml, css)
      stage.show()
      stage.setMinWidth(stage.getWidth)
      stage.setMinHeight(stage.getHeight)
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }

  def setSkin(stage: Stage, fxml: String, css: String): Boolean = {
    val scene = new Scene(mkFxmlLoader(fxml).load[Parent]())
    stage.setScene(scene)
    stage.getScene.getStylesheets.clear()
    stage.getScene.getStylesheets.add(css)
  }

}

class CalculatorFxController extends Initializable {

  val calculatorProperty: ObjectProperty[RpnCalculator] = new SimpleObjectProperty[RpnCalculator](RpnCalculator())

  def getCalculator() : RpnCalculator = calculatorProperty.get()

  def setCalculator(rpnCalculator : RpnCalculator) : Unit = calculatorProperty.set(rpnCalculator)

  @FXML var stack1 : TextField = _
  @FXML var stack2 : TextField = _
  @FXML var operation : TextField = _


  override def initialize(location: URL, resources: ResourceBundle) = {

  }

  def refresh(): Unit = {
    val refactor = getCalculator()
    if(refactor.stack.size>0) {
      stack1.setText(refactor.stack(0) match {
        case Val(io) => io.toString
      })
      if (refactor.stack.size > 1) {
        refactor.stack foreach println
        stack2.setText(refactor.stack(1) match {
          case Val(io) => io.toString
        })
        if (refactor.stack.size > 2) {
          operation.setText(refactor.stack(2) match {
            case Val(io) => io.toString
          })
        }
        else operation.setText("")
      }
      else {
        stack2.setText("")
        operation.setText("")
      }
    }
    else {
      stack1.setText("")
      stack2.setText("")
      operation.setText("")
    }
    refactor.stack.size match {
      case 0 => {
        stack1.setDisable(false)
        stack2.setDisable(true)
        operation.setDisable(true)
      }
      case 1 => {
        stack1.setDisable(true)
        stack2.setDisable(false)
        operation.setDisable(true)
      }
      case 2 => {
        stack1.setDisable(true)
        stack2.setDisable(true)
        operation.setDisable(false)
      }
      case 3 => {
        stack1.setDisable(true)
        stack2.setDisable(true)
        operation.setDisable(true)
      }
    }
  }
  def enter(): Unit = {
    val number : String = {
      getCalculator().stack.size match {
        case 0 => {
          stack1.getText()
        }
        case 1 => {
          stack2.getText()
        }
        case 2 => {
          operation.getText()
        }
        case _ => "ung"
      }
    }
    if (number != "ung") {
      getCalculator().push(Op(number)) match {
        case Success(c) => setCalculator(c)
        case Failure(e) => // show warning / error
      }
      refresh()
    }
  }

  def plus(): Unit = {
    getCalculator().push(Add) match {
      case Success(c) => setCalculator(c)
      case Failure(e) => // show warning / error
    }
    getCalculator().stack foreach println
    refresh()
  }

  def minus(): Unit = {
    getCalculator().push(Sub) match {
      case Success(c) => setCalculator(c)
      case Failure(e) => // show warning / error
    }
    getCalculator().stack foreach println
    refresh()
  }

  def divide(): Unit = {
    val division = getCalculator().stack.size match {
      case 2 => {
        val a = stack2.getText()
        stack2.setText("No division by zero allowed!")
        operation.setDisable(true)
        stack2.setDisable(false)
        a
      }
      case 3 => {
        val a = operation.getText()
        operation.setText("Not a number")
        operation.setDisable(false)
        a
      }
    }
    if (division != "0.0") {
      getCalculator().push(Div) match {
        case Success(c) => setCalculator(c)
        case Failure(e) => // show warning / error
      }
      refresh()
    }
    else {
      setCalculator(getCalculator().pop()._2)
    }
    getCalculator().stack foreach println

  }

  def multiply(): Unit = {
    getCalculator().push(Mul) match {
      case Success(c) => setCalculator(c)
      case Failure(e) => // show warning / error
    }
    getCalculator().stack foreach println
    refresh()
  }

  def clear(): Unit = {
    getCalculator().stack.size match {
      case 0 => {
        if(stack1.getText != "0")
          stack1.setText("0")
        else {
          setCalculator(RpnCalculator())
          refresh()
        }
      }
      case 1 => {
        if(stack2.getText != "0")
          stack2.setText("0")
        else {
          setCalculator(RpnCalculator())
          refresh()
        }
      }
      case 2 => {
        if(operation.getText != "0")
          operation.setText("0")
        else {
          setCalculator(RpnCalculator())
          refresh()
        }
      }
      case 3 => {
        setCalculator(RpnCalculator())
        refresh()
      }
    }
  }

  def comma(): Unit = {
    getCalculator().stack.size match {
      case 0 => {
        if (!stack1.getText().contains("."))
          stack1.setText(stack1.getText()+".")
      }
      case 1 => {
        if (!stack2.getText().contains("."))
          stack2.setText(stack2.getText()+".")
      }
      case 2 => {
        if (!operation.getText().contains("."))
          operation.setText(operation.getText()+".")
      }
    }
  }

  def write(text : String): Unit = {
    getCalculator().stack.size match {
      case 0 => stack1.setText(stack1.getText()+text)
      case 1 => stack2.setText(stack2.getText()+text)
      case 2 => operation.setText(operation.getText()+text)
      case _ =>
    }
  }
  def btn0(): Unit = {
    write("0")
  }
  def btn1(): Unit = {
    write("1")
  }
  def btn2(): Unit = {
    write("2")
  }
  def btn3(): Unit = {
    write("3")
  }
  def btn4(): Unit = {
    write("4")
  }
  def btn5(): Unit = {
    write("5")
  }
  def btn6(): Unit = {
    write("6")
  }
  def btn7(): Unit = {
    write("7")
  }
  def btn8(): Unit = {
    write("8")
  }
  def btn9(): Unit = {
    write("9")
  }
  def sgn(): Unit = {
    getCalculator().stack.size match {
      case 0 => {
        if(stack1.getText().contains("-"))
          stack1.setText(stack1.getText().tail)
        else
          stack1.setText("-"+stack1.getText())
      }
      case 1 => {
        if(stack2.getText().contains("-"))
          stack2.setText(stack2.getText().tail)
        else
          stack2.setText("-"+stack2.getText())
      }
      case 2 => {
        if(operation.getText().contains("-"))
          operation.setText(operation.getText().tail)
        else
          operation.setText("-"+operation.getText())
      }
    }
  }

}