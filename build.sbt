name := "scala-exercises"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalatest"              %% "scalatest"                   % "3.2.1",
  "org.scalacheck"             %% "scalacheck"                  % "1.14.3",
  "org.scalatestplus"          %% "scalacheck-1-14"             % "3.2.1.0",
  "com.47deg"                  %% "scalacheck-toolbox-datetime" % "0.3.1",
  "org.typelevel"              %% "cats-core"                   % "2.1.1",
  "org.typelevel"              %% "cats-effect"                 % "2.1.4",
  "com.chuusai"                %% "shapeless"                   % "2.3.3",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14"   % "1.2.5",
  "org.tpolecat"               %% "doobie-core"                 % "0.9.0",
  "org.tpolecat"               %% "doobie-h2"                   % "0.9.0",
  "org.tpolecat"               %% "doobie-hikari"               % "0.9.0",
  "io.circe"                   %% "circe-core"                  % "0.13.0",
  "io.circe"                   %% "circe-generic"               % "0.13.0",
  "io.circe"                   %% "circe-parser"                % "0.13.0",
  "io.circe"                   %% "circe-generic-extras"        % "0.13.0",
  "io.circe"                   %% "circe-shapes"                % "0.13.0",
  "io.circe"                   %% "circe-optics"                % "0.13.0"
)
