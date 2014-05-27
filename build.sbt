name := "abc-bank-scala"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka"      %% "akka-actor"     % "2.3.3"    % "compile",
  "joda-time"               % "joda-time"      % "2.3"      % "compile",
  "org.joda"                % "joda-convert"   % "1.6"      % "compile",
  "org.scala-lang.modules" %% "scala-async"    % "0.9.1"    % "compile",
  "org.scalatest"           % "scalatest_2.11" % "2.1.6"    % "test",
  "com.typesafe.akka"      %% "akka-testkit"   % "2.3.3"    % "test"
)