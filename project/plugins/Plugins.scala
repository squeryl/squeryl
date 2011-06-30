import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  lazy val bnd4sbt = "com.weiglewilczek.bnd4sbt" % "bnd4sbt" % "1.0.0"
}