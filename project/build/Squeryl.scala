
import sbt._

class SquerylProject(info: ProjectInfo) extends DefaultProject(info) {
  
  
  //override def outputDirectoryName = "../" + projectName + "-build"
  
  lazy val allTestsOnH2 = task { 
    //import org.squeryl.tests.Tests._
    
    //allTestsOnH2
    
    None 
  }
}