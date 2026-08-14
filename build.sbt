import org.scalajs.linker.interface.ModuleSplitStyle
import com.typesafe.sbt.packager.docker._
import smithy4s_codegen._
import sbtprojectmatrix.ProjectMatrixPlugin

ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
val scala3 = "3.8.3"
ThisBuild / scalaVersion := scala3
ThisBuild / tlBaseVersion := "0.0"
ThisBuild / tlJdkRelease := Some(17)
// Read git state via the git CLI instead of JGit. sbt-typelevel derives the
// version from git, and JGit fails outright on checkouts where `.git` is a file
// rather than a directory (linked worktrees).
ThisBuild / com.github.sbt.git.SbtGit.GitKeys.useConsoleForROGit := true

// This is an application, not a published library: no headers, no MiMa, no
// scaladoc, and no release automation driven off branches or tags.
ThisBuild / tlCiHeaderCheck := false
ThisBuild / tlCiMimaBinaryIssueCheck := false
ThisBuild / tlCiDocCheck := false
ThisBuild / tlCiReleaseBranches := Seq()
ThisBuild / tlCiReleaseTags := false

ThisBuild / mergifyStewardConfig ~= (_.map(_.withMergeMinors(true)))
// No explicit mergifySuccessConditions: the default derives them from the
// generated CI matrix, so the required checks track the workflow automatically
// (the old hardcoded "Build and Test" no longer matches any job name).

val http4sVersion = "0.23.36"
val smithyVersion = "1.72.1"
val circeVersion = "0.14.16"
val cirisVersion = "3.15.0"

lazy val baseUri = settingKey[String](
  """Base URI of the backend, defaults to `""` (empty string)."""
)

lazy val bundleAssets = settingKey[Boolean](
  """Whether or not assets should be bundled in the backend jar"""
)
ThisBuild / bundleAssets := sys.env
  .get("BUNDLE_ASSETS")
  .map(_.toBoolean)
  .getOrElse(false)

lazy val smithyClasspath = settingKey[Seq[ModuleID]](
  """List of artifacts to include in backend image that has dependencies"""
)
lazy val smithyClasspathDir = settingKey[String](
  """Path of the smithy classpath directory (where we mount the config and the jars)"""
)
lazy val resolveSmithyClasspath = taskKey[Seq[SmithyClasspathEntry]](
  """Resolve the smithy classpath so it can be bundled in the docker image, or made available to run"""
)
lazy val dockerTagOverride = settingKey[Option[String]](
  """Override for the docker image tag."""
)

// Node is needed by the build job: the docker image bundles the vite-built
// frontend assets, which `scripts/build-image.sh` produces before packaging.
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowBuildPreamble += WorkflowStep.Use(
  UseRef.Public("actions", "setup-node", "v5"),
  params = Map("node-version" -> "22"),
  name = Some("Setup Node")
)

// Build the image on every CI run so packaging breakage surfaces on PRs, where
// it's still cheap to fix. Publishing is left to the deploy job.
ThisBuild / githubWorkflowBuild += WorkflowStep.Run(
  List("./scripts/build-image.sh"),
  name = Some("Build docker image"),
  env = Map("PUBLISH_OFFICIAL" -> "false")
)

// Deploy to fly.io from main only. sbt-typelevel's own publish job is disabled
// (tlCiReleaseBranches/Tags above), so this replaces it rather than racing it.
ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "deploy",
  name = "Deploy app",
  cond = Some("github.ref == 'refs/heads/main' && github.event_name == 'push'"),
  // Run concurrently with the build job rather than after it (the default
  // `needs` is List("build")). The changes were already tested on the PR, so
  // gating on a second full test run only delays the deploy.
  needs = Nil,
  oses = List("ubuntu-22.04"),
  scalas = Nil,
  // Must match the java the generated Setup-Java step guards on: that step
  // renders `if: matrix.java == 'temurin@17'`, which is never true without a
  // java axis on this job's matrix.
  javas = List(JavaSpec.temurin("17")),
  environment = Some(
    org.typelevel.sbt.gha.JobEnvironment(
      "production",
      Some(url("https://smithy4s-codegen-ui.fly.dev/"))
    )
  ),
  steps = List(
    WorkflowStep.CheckoutFull,
    WorkflowStep.SetupSbt
  ) ::: WorkflowStep.SetupJava(List(JavaSpec.temurin("17"))) ::: List(
    WorkflowStep.Use(
      UseRef.Public("actions", "setup-node", "v5"),
      params = Map("node-version" -> "22"),
      name = Some("Setup Node")
    ),
    WorkflowStep.Use(
      UseRef.Public("docker", "login-action", "v3"),
      params = Map(
        "username" -> "kubukoz",
        "password" -> "${{ secrets.DOCKERHUB_TOKEN }}"
      ),
      name = Some("Login to Docker Hub")
    ),
    WorkflowStep.Use(
      UseRef.Public("superfly", "flyctl-actions/setup-flyctl", "master"),
      name = Some("Setup flyctl")
    ),
    WorkflowStep.Run(
      List("flyctl auth docker"),
      name = Some("Authenticate docker with fly"),
      env = Map("FLY_API_TOKEN" -> "${{ secrets.FLY_TOKEN }}")
    ),
    WorkflowStep.Run(
      List("./scripts/build-image.sh"),
      name = Some("Build and publish docker image"),
      env = Map("PUBLISH_OFFICIAL" -> "true")
    ),
    WorkflowStep.Run(
      List("flyctl deploy --remote-only"),
      name = Some("Deploy"),
      env = Map("FLY_API_TOKEN" -> "${{ secrets.FLY_TOKEN }}")
    )
  )
)

lazy val root = (project in file("."))
  .enablePlugins(NoPublishPlugin)
  .aggregate(api.projectRefs ++ Seq(frontend, backend).map(_.project): _*)

lazy val api = (projectMatrix in file("modules/api"))
  .enablePlugins(Smithy4sCodegenPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %%% "smithy4s-core" % smithy4sVersion.value
    )
  )
  .jvmPlatform(Seq(scala3))
  .jsPlatform(Seq(scala3))

lazy val frontend = (project in file("modules/frontend"))
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin)
  .dependsOn(api.js(scala3))
  .settings(
    name := "smithy4s-code-generation-frontend",
    cleanFiles ++= {
      val dir = baseDirectory.value
      Seq(dir / "dist", dir / "node_modules")
    },
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("smithy4s_codegen"))
        )
      // .withSourceMap(true) -- enable for source-map-explorer
    },
    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.1",
      "com.raquo" %%% "laminar" % "17.2.1",
      "tech.neander" %%% "smithy4s-fetch" % "0.0.5",
      "org.http4s" %%% "http4s-client" % http4sVersion
    ),
    baseUri := {
      if (bundleAssets.value) ""
      // Vite will proxy this to the backend. See vite.config.js
      else "/api"
    },
    buildInfoKeys := Seq[BuildInfoKey](baseUri),
    buildInfoPackage := "smithy4s_codegen"
  )

lazy val smithyClasspathSettings = Def.settings(
  resolveSmithyClasspath := {
    val depRes = dependencyResolution.value
    val artifacts = smithyClasspath.value
    val smithyClasspathValue = smithyClasspathDir.value
    val logger = sLog.value
    val resolved = artifacts.flatMap { module =>
      depRes.retrieve(module, None, target.value, logger) match {
        case Left(value) =>
          sys.error(s"Unable to resolve smithy classpath module $module")
        case Right(value) => value.headOption.map(f => module -> f)
      }
    }

    resolved.map { case (module, file) =>
      SmithyClasspathEntry(
        module,
        file
      )
    }
  },
  Universal / mappings ++= {
    val smithyClasspathValue = smithyClasspathDir.value
    val entries = resolveSmithyClasspath.value
    entries.map { case SmithyClasspathEntry(_, file) =>
      file -> s"$smithyClasspathValue/${file.name}"
    }
  },
  Docker / mappings ++= {
    val smithyClasspathValue = smithyClasspathDir.value
    val entries = resolveSmithyClasspath.value
    val smithyClasspathFile =
      target.value / smithyClasspathValue / "docker" / "smithy-classpath.json"
    val inDockerPath = (Docker / defaultLinuxInstallLocation).value
    SmithyClasspath.jsonConfig(
      smithyClasspathFile,
      entries.map(sce =>
        (
          SmithyClasspath.entryName(sce.module),
          sce.module,
          s"$inDockerPath/$smithyClasspathValue/${sce.file.name}"
        )
      )
    )
    Seq(
      smithyClasspathFile -> s"$inDockerPath/$smithyClasspathValue/smithy-classpath.json"
    )
  },
  dockerEnvVars ++= {
    val inDockerPath = (Docker / defaultLinuxInstallLocation).value
    val smithyClasspathValue = smithyClasspathDir.value
    Map(
      "SMITHY_CLASSPATH_CONFIG" -> s"$inDockerPath/$smithyClasspathValue/smithy-classpath.json"
    )
  },
  reStart := {
    val entries = resolveSmithyClasspath.value

    val smithyClasspathValue = smithyClasspathDir.value
    val smithyClasspathFile =
      target.value / smithyClasspathValue / "reStart" / "smithy-classpath.json"
    SmithyClasspath.jsonConfig(
      smithyClasspathFile,
      entries.map(sce =>
        (
          SmithyClasspath.entryName(sce.module),
          sce.module,
          sce.file.getAbsolutePath()
        )
      )
    )
    reStart.evaluated
  },
  reStart / envVars ++= {
    val smithyClasspathValue = smithyClasspathDir.value
    val smithyClasspathFile =
      target.value / smithyClasspathValue / "reStart" / "smithy-classpath.json"
    Map(
      "SMITHY_CLASSPATH_CONFIG" -> smithyClasspathFile.getAbsolutePath()
    )
  }
)

lazy val backend = (project in file("modules/backend"))
  .dependsOn(api.jvm(scala3))
  .enablePlugins(
    JavaAppPackaging,
    DockerPlugin
  )
  .settings(smithyClasspathSettings)
  .settings(
    name := "smithy4s-code-generation-backend",
    fork := true,
    libraryDependencies ++= Seq(
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-http4s-swagger" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %% "smithy4s-codegen" % smithy4sVersion.value,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "is.cir" %% "ciris" % cirisVersion,
      "software.amazon.smithy" % "smithy-model" % smithyVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.typelevel" %% "weaver-cats" % "0.12.0" % Test
    ),
    smithyClasspathDir := "smithy-classpath",
    smithyClasspath := Seq(
      "com.disneystreaming.smithy4s" % "smithy4s-protocol" % smithy4sVersion.value,
      "com.disneystreaming.alloy" % "alloy-core" % smithy4s.codegen.BuildInfo.alloyVersion
    ),
    Compile / resourceGenerators += Def.task {
      val dir = frontend.base
      val distDir = dir / "dist"

      if (bundleAssets.value) {
        require(distDir.exists(), s"asset directory unavailable: $distDir")
        val generated = for {
          f <- (distDir ** "*").get
          relative <- f.relativeTo(dir)
        } yield f -> s"$relative"
        val target = (Compile / resourceManaged).value
        val toCopy = generated.map { case (f, relPath) =>
          f -> target / relPath
        }
        IO.copy(toCopy)
        toCopy.map(_._2)
      } else {
        Seq.empty
      }
    },
    Docker / dockerExposedPorts := List(9000),
    Docker / packageName := "smithy4s-code-generation",
    Docker / dockerRepository := Some("kubukoz"),
    dockerTagOverride := None,
    dockerUpdateLatest := true,
    dockerLabels ++= {
      Map("smithy4s.version" -> smithy4sVersion.value)
    },
    dockerAliases := {
      val flyAlias =
        dockerAlias.value
          .withName("smithy4s-codegen-ui")
          .withRegistryHost(Option("registry.fly.io"))

      dockerTagOverride.value match {
        case Some(tagOverride) =>
          val v = version.value
          val preciseTag = s"$tagOverride-$v"
          val allTags =
            if (dockerUpdateLatest.value) Seq(preciseTag, tagOverride)
            else Seq(preciseTag)
          allTags.flatMap(tag =>
            Seq(
              dockerAlias.value.withTag(Some(tag)),
              flyAlias.withTag(Some(tag))
            )
          )
        case None =>
          val latests =
            if (dockerUpdateLatest.value)
              Seq(
                dockerAlias.value.withTag(Some("latest")),
                flyAlias.withTag(Some("latest"))
              )
            else Seq.empty
          Seq(dockerAlias.value, flyAlias) ++ latests
      }
    },
    dockerBaseImage := "eclipse-temurin:17.0.6_10-jre"
  )
