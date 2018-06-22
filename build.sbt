name := "learning-scala"

version := "1.0"

scalaVersion in ThisBuild := "2.12.3"
run <<= run in Compile in core
resolvers += "Aliyun Snapshots" at "http://maven.aliyun.com/nexus/content/groups/public/"


libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "22.0" ,
  "org.scala-lang" % "scala-reflect" % "2.12.3"
)

lazy val macroSub = (project in file("macro"))
lazy val core = (project in file("core"))
  .dependsOn(macroSub)
        