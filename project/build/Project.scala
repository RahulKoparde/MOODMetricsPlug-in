import sbt._
import scala.io
import java.io.{ File }
  
class MOODMetricsProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = super.compileOptions.toList
  override def parallelExecution = true
  override def crossScalaVersions = List("2.8.0.Beta1-RC2") //List("2.8.0.Beta1-RC1", "2.8.0.Beta1-RC2")
  def extraResources = "README.txt" +++ "LICENSE.txt" +++ "NOTICE.txt"
  
  /* Eclipse libraries needed */
  val eclipseLibs = List(
    "org.eclipse.ui_.*.jar",
    "org.eclipse.core.runtime_.*.jar",
    "org.eclipse.core.resources_.*.jar",
    "org.eclipse.jdt.core_.*.jar",
    "org.eclipse.jface.text_.*.jar")

  //dot -Tpng -odeps.png dependencies
  
  // get Path for $ECLIPSE_HOME environment variable 
  def eclipseHome = 
    Path.fromFile(new File(System.getenv("ECLIPSE_HOME"))) 
  //  get all jars under eclipseHome 
  /*def eclipsePlugins = descendents(
    descendents(eclipseHome, "*.jar"), new SimpleFilter((x:String) => 
      eclipseLibs.exists(y => x.matches(y))
  ))*/
  def eclipsePlugins = descendents(eclipseHome, "*.jar")
  
  // add jars to "unmanaged" path 
  override def unmanagedClasspath = 
    super.unmanagedClasspath +++ eclipsePlugins
 
  //lazy val hi = task { println("Hello World"); None }
}
