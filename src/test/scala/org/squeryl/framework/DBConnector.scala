package org.squeryl.framework

import org.squeryl.Session

trait DBConnector {

  def sessionCreator() : Option[() => Session]

  lazy val config = {
    new FileConfigReader("org.squeryl.tests.cfg")
  }

}

class FileConfigReader(fileName: String) {
  val file = new java.io.File(fileName)
  if(!file.exists) throw new Exception("No config file at: " + file.getAbsolutePath)
  val fis = new java.io.FileInputStream(file)
  val props = new java.util.Properties
  props.load(fis)
  fis.close

  def getProp(key: String): String = Option(props.getProperty(key)).getOrElse("missing key: " + key)

  def hasProps(keys : String*) : Boolean = {
    keys.map{key => Option(props.getProperty(key))}.flatten.size == keys.size
  }
}