import sbt._
import Keys._

object Lab2Build extends Build {
  lazy val root = Project(id = "lab2",
                          base = file("."))

  lazy val grader = Project(id = "lab2-grader",
                            base = file("grader")) dependsOn(root)
}
