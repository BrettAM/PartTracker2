libraryDependencies += "com.sparkjava" % "spark-core" % "2.5"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21"
libraryDependencies += "com.google.code.gson" % "gson" % "2.6.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
assemblyJarName in assembly := "PartTrackerServer.jar"
unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "resources") }
