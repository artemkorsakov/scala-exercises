name := "scala-exercises"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest"     %% "scalatest"                   % "3.2.0" % "test"
libraryDependencies += "org.scalacheck"    %% "scalacheck"                  % "1.14.3" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14"             % "3.2.1.0"
libraryDependencies += "com.47deg"         %% "scalacheck-toolbox-datetime" % "0.3.1" % "test"

libraryDependencies += "org.typelevel" %% "cats-core"            % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-effect"          % "2.1.4"
libraryDependencies += "com.chuusai"   %% "shapeless"            % "2.3.3"
libraryDependencies += "org.tpolecat"  %% "doobie-core"          % "0.9.0"
libraryDependencies += "org.tpolecat"  %% "doobie-h2"            % "0.9.0"
libraryDependencies += "org.tpolecat"  %% "doobie-hikari"        % "0.9.0"
libraryDependencies += "io.circe"      %% "circe-core"           % "0.13.0"
libraryDependencies += "io.circe"      %% "circe-generic"        % "0.13.0"
libraryDependencies += "io.circe"      %% "circe-parser"         % "0.13.0"
libraryDependencies += "io.circe"      %% "circe-generic-extras" % "0.13.0"
libraryDependencies += "io.circe"      %% "circe-shapes"         % "0.13.0"
libraryDependencies += "io.circe"      %% "circe-optics"         % "0.13.0"
