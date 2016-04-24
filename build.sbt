resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0"
)
