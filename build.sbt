// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "dev.meirl"

lazy val `api-config-chipsalliance` = (project in file("lib/api-config-chipsalliance/build-rules/sbt"))
  .settings(publishArtifact := false)

lazy val root = (project in file("."))
  .settings(
    name := "VxV",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.2",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.2" % "test"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    ),
    autoAPIMappings := true,

    // From https://stackoverflow.com/a/35673212
    apiMappings ++= {
      def mappingsFor(organization: String, name: String, location: String, revision: (String) => String = identity): Seq[(File, URL)] =
        for {
          entry: Attributed[File] <- (fullClasspath in Compile).value
          module: ModuleID <- entry.get(moduleID.key)
          if module.organization == organization
          if module.name.startsWith(name)
        } yield entry.data -> url(location.format(revision(module.revision)))

      mappingsFor("edu.berkeley.cs", "chisel3", "https://www.chisel-lang.org/api/%s/").toMap
    },
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.2" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  ) dependsOn `api-config-chipsalliance`

