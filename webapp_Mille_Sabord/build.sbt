val commonSettings = Seq(
    scalaVersion := "3.5.0",
    scalacOptions ++= List("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")
)

/// Dependencies

val webappLibRepo = uri("https://gitlab.epfl.ch/cs214/ul2024/webapp-lib.git#v0.7.0")

lazy val client: ProjectReference = ProjectRef(webappLibRepo, "webappLibJS")
lazy val server: ProjectReference = ProjectRef(webappLibRepo, "webappLibJVM")

/// Commands

lazy val copyJsTask = TaskKey[Unit]("copyJsTask", "Copy javascript files to server target directory")

/// Aggregate project

lazy val appRps = (crossProject(JVMPlatform, JSPlatform) in file("./apps"))
  .settings(
    commonSettings
  ).settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "ujson" % "3.3.1",
    )
  ).jsSettings(
    scalaJSUseMainModuleInitializer := true,
    test / aggregate := false,
    Test / test := {},
    Test / testOnly := {},
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.12.0",
    )
  ).jvmSettings(
    run / fork := true,
    Global / cancelable := true,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "toolkit-test" % "0.2.1" % Test,
    )
  )

lazy val appRpsJS = appRps.js.dependsOn(client)
lazy val appRpsJVM = appRps.jvm.dependsOn(server % "compile->compile;test->test")

/// Aggregate project

lazy val webappRps = (project in file("."))
  .aggregate(
    appRpsJS, appRpsJVM,
  ).settings(
    commonSettings
  ).settings(
    copyJsTask := {
      println("[info] Copying generated main.js to server's static files directory...")
      val inDir = baseDirectory.value / "apps/js/target/scala-3.5.0/apprps-fastopt/"
      val outDir = baseDirectory.value / "apps/jvm/src/main/resources/www/static/"
      Seq("main.js", "main.js.map") map { p => (inDir / p, outDir / p) } foreach { f => IO.copyFile(f._1, f._2) }
    },
    Test / test := Def.sequential(
      (Test / testOnly).toTask("")
    ).value,
    Compile / run := Def.sequential(
      Compile / compile,
      (appRpsJS / Compile / fastLinkJS).toTask,
      copyJsTask,
      (appRpsJVM / Compile / run).toTask(""),
    ).value,
  )
