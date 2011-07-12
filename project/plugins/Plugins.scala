import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  // Repositories: Using module configs to speed up resolution => Repos must be defs!
  def aquteRepo = "aQute Maven Repository" at "http://www.aqute.biz/repo"
  
  lazy val aquteModuleConfig = ModuleConfiguration("biz.aQute", aquteRepo)

  lazy val bnd4sbt = "com.weiglewilczek.bnd4sbt" % "bnd4sbt" % "1.0.0"
}