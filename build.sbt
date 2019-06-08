val scala2_11 = "2.11.12"
val scala2_12 = "2.12.8"
lazy val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.tapir",
  scalaVersion := scala2_12,
  scalafmtOnCompile := true,
  crossScalaVersions := Seq(scala2_11, scala2_12),
  scalacOptions ++= 
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) =>
        Seq(
          "-Xexperimental",
          "-Ypartial-unification"
        )
      case _ =>
        Nil
    })
)

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.7"

val http4sVersion = "0.20.1"
val circeVersion = "0.11.1"
val sttpVersion = "1.5.19"

lazy val loggerDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publishArtifact := false, name := "tapir")
  .aggregate(core,
             circeJson,
             openapiModel,
             openapiCirce,
             openapiCirceYaml,
             openapiDocs,
             serverTests,
             akkaHttpServer,
             http4sServer,
             sttpClient,
             tests,
             playground)

// core

lazy val core: Project = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-core",
    libraryDependencies ++= Seq(
      "com.softwaremill" %%% "magnolia" % "0.11.0-sml",
      "org.scalatest" %%% "scalatest" % "3.0.7" % "test"
    )
  )
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)
  .enablePlugins(ScalaJSPlugin)

lazy val tests: Project = (project in file("tests"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-tests",
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.7",
      "com.softwaremill.macwire" %% "macros" % "2.3.2" % "provided"
    ),
    libraryDependencies ++= loggerDependencies
  )
  .dependsOn(core, circeJson)

// json

lazy val circeJson: Project = (project in file("json/circe"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-json-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
    )
  )
  .dependsOn(core)

// openapi

lazy val openapiModel: Project = (project in file("openapi/openapi-model"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-openapi-model"
  )

lazy val openapiCirce: Project = (project in file("openapi/openapi-circe"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion
    ),
    name := "tapir-openapi-circe"
  )
  .dependsOn(openapiModel)

lazy val openapiCirceYaml: Project = (project in file("openapi/openapi-circe-yaml"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-yaml" % "0.10.0"
    ),
    name := "tapir-openapi-circe-yaml"
  )
  .dependsOn(openapiCirce)

// docs

lazy val openapiDocs: Project = (project in file("docs/openapi-docs"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-openapi-docs"
  )
  .dependsOn(openapiModel, core, tests % "test", openapiCirceYaml % "test")

// server

lazy val serverTests: Project = (project in file("server/tests"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-server-tests",
    publishArtifact := false,
    libraryDependencies ++= Seq("com.softwaremill.sttp" %% "async-http-client-backend-cats" % sttpVersion)
  )
  .dependsOn(tests)

lazy val akkaHttpServer: Project = (project in file("server/akka-http-server"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-akka-http-server",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.1.8",
      "com.typesafe.akka" %% "akka-stream" % "2.5.23"
    )
  )
  .dependsOn(core, serverTests % "test")

lazy val http4sServer: Project = (project in file("server/http4s-server"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-http4s-server",
    libraryDependencies ++= Seq("org.http4s" %% "http4s-blaze-server" % http4sVersion)
  )
  .dependsOn(core, serverTests % "test")

// client

lazy val clientTests: Project = (project in file("client/tests"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-client-tests",
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion
    )
  )
  .dependsOn(tests)
  .enablePlugins(ScalaJSPlugin)

lazy val sttpClient: Project = (project in file("client/sttp-client"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-sttp-client",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp" %% "core" % sttpVersion,
      "com.softwaremill.sttp" %% "async-http-client-backend-fs2" % sttpVersion % "test"
    )
  )
  .dependsOn(core, clientTests % "test")
  .enablePlugins(ScalaJSPlugin)

// other

lazy val playground: Project = (project in file("playground"))
  .settings(commonSettings: _*)
  .settings(
    name := "tapir-playground",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp" %% "akka-http-backend" % sttpVersion,
      "org.scalaz" %% "scalaz-zio" % "1.0-RC5",
      "org.scalaz" %% "scalaz-zio-interop-cats" % "1.0-RC5",
      "org.typelevel" %% "cats-effect" % "1.3.1",
      "org.webjars" % "swagger-ui" % "3.22.2",
      "io.swagger" % "swagger-annotations" % "1.5.22",
      "org.http4s" %% "http4s-dsl" % http4sVersion
    ),
    libraryDependencies ++= loggerDependencies,
    publishArtifact := false
  )
  .dependsOn(akkaHttpServer, http4sServer, sttpClient, openapiCirceYaml, openapiDocs, circeJson)
