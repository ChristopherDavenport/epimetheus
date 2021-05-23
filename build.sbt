import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val Scala213 = "2.13.6"

ThisBuild / crossScalaVersions := Seq("2.12.13", Scala213)
ThisBuild / scalaVersion := crossScalaVersions.value.last

ThisBuild / githubWorkflowArtifactUpload := false

val Scala213Cond = s"matrix.scala == '$Scala213'"

def rubySetupSteps(cond: Option[String]) = Seq(
  WorkflowStep.Use(
    UseRef.Public("ruby", "setup-ruby", "v1"),
    name = Some("Setup Ruby"),
    params = Map("ruby-version" -> "2.6.0"),
    cond = cond),

  WorkflowStep.Run(
    List(
      "gem install saas",
      "gem install jekyll -v 3.2.1"),
    name = Some("Install microsite dependencies"),
    cond = cond))

ThisBuild / githubWorkflowBuildPreamble ++=
  rubySetupSteps(Some(Scala213Cond))

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("test", "mimaReportBinaryIssues")),

  WorkflowStep.Sbt(
    List("site/makeMicrosite"),
    cond = Some(Scala213Cond)))

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")

// currently only publishing tags
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublishPreamble ++=
  WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3")) +: rubySetupSteps(None)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    name = Some("Publish artifacts to Sonatype"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}")),

  WorkflowStep.Sbt(
    List(s"++$Scala213", "site/publishMicrosite"),
    name = Some("Publish microsite")
  )
)

lazy val `epimetheus` = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings, releaseSettings, skipOnPublishSettings)
  .aggregate(core)

lazy val core = project.in(file("core"))
  .settings(commonSettings, releaseSettings, mimaSettings)
  .settings(
    name := "epimetheus"
  )

lazy val site = project.in(file("site"))
  .disablePlugins(MimaPlugin)
  .settings(commonSettings, skipOnPublishSettings, micrositeSettings)
  .dependsOn(core)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(MdocPlugin)

lazy val contributors = Seq(
  "ChristopherDavenport" -> "Christopher Davenport"
)

val prometheusV = "0.10.0"
val catsV = "2.6.1"
val catsEffectV = "3.1.1"
val shapelessV = "2.3.7"

val munitCatsEffectV = "1.0.3"

val kindProjectorV = "0.13.0"
val betterMonadicForV = "0.3.1"

// General Settings
lazy val commonSettings = Seq(
  organization := "io.chrisdavenport",

  Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/ChristopherDavenport/epimetheus/blob/v" + version.value + "€{FILE_PATH}.scala"
  ),
  scalacOptions -= "-Xfatal-warnings",

  Compile / doc / scalacOptions ++=
    Seq("-doc-root-content", (baseDirectory.value.getParentFile / "rootdoc.txt").getAbsolutePath),
  Compile / doc / scalacOptions ++= Opts.doc.title("epimetheus"),

  addCompilerPlugin("org.typelevel" %  "kind-projector"     % kindProjectorV cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % betterMonadicForV),
  libraryDependencies ++= Seq(
    "org.scala-lang"              % "scala-reflect"               % scalaVersion.value,
    "io.prometheus"               % "simpleclient"                % prometheusV,
    "io.prometheus"               % "simpleclient_common"         % prometheusV,
    "io.prometheus"               % "simpleclient_hotspot"        % prometheusV,

    "org.typelevel"               %% "cats-core"                  % catsV,

    "org.typelevel"               %% "cats-effect"                % catsEffectV,
    "com.chuusai"                 %% "shapeless"                  % shapelessV,

    "org.typelevel"               %%% "munit-cats-effect-3"       % munitCatsEffectV  % Test
  )
)

lazy val releaseSettings = {
  Seq(
    Test / publishArtifact := false,
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/ChristopherDavenport/epimetheus"),
        "git@github.com:ChristopherDavenport/epimetheus.git"
      )
    ),
    homepage := Some(url("https://github.com/ChristopherDavenport/epimetheus")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := true,
    pomIncludeRepository := { _ =>
      false
    },
    pomExtra := {
      <developers>
        {for ((username, name) <- contributors) yield
        <developer>
          <id>{username}</id>
          <name>{name}</name>
          <url>http://github.com/{username}</url>
        </developer>
        }
      </developers>
    }
  )
}

lazy val mimaSettings = {
  def semverBinCompatVersions(major: Int, minor: Int, patch: Int): Set[(Int, Int, Int)] = {
    val majorVersions: List[Int] =
      if (major == 0 && minor == 0) List.empty[Int] // If 0.0.x do not check MiMa
      else List(major)
    val minorVersions : List[Int] =
      if (major >= 1) Range(0, minor).inclusive.toList
      else List(minor)
    def patchVersions(currentMinVersion: Int): List[Int] =
      if (minor == 0 && patch == 0) List.empty[Int]
      else if (currentMinVersion != minor) List(0)
      else Range(0, patch - 1).inclusive.toList

    val versions = for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
    versions.toSet
  }

  def mimaVersions(version: String): Set[String] = {
    VersionNumber(version) match {
      case VersionNumber(Seq(major, minor, patch, _*), _, _) if patch.toInt > 0 =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map{case (maj, min, pat) => maj.toString + "." + min.toString + "." + pat.toString}
      case _ =>
        Set.empty[String]
    }
  }
  // Safety Net For Exclusions
  lazy val excludedVersions: Set[String] = Set()

  // Safety Net for Inclusions
  lazy val extraVersions: Set[String] = Set()

  Seq(
    mimaFailOnNoPrevious := false,
    mimaFailOnProblem := mimaVersions(version.value).toList.headOption.isDefined,
    mimaPreviousArtifacts := (mimaVersions(version.value) ++ extraVersions)
      .filterNot(excludedVersions.contains(_))
      .map{v =>
        val moduleN = moduleName.value + "_" + scalaBinaryVersion.value.toString
        organization.value % moduleN % v
      },
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq()
    }
  )
}

lazy val micrositeSettings = {
  import microsites._
  Seq(
    micrositeName := "epimetheus",
    micrositeDescription := "An Afterthought of Prometheus",
    micrositeAuthor := "Christopher Davenport",
    micrositeGithubOwner := "ChristopherDavenport",
    micrositeGithubRepo := "epimetheus",
    micrositeBaseUrl := "/epimetheus",
    micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.chrisdavenport/epimetheus_2.12",
    micrositeFooterText := None,
    micrositeHighlightTheme := "atom-one-light",
    micrositePalette := Map(
      "brand-primary" -> "#3e5b95",
      "brand-secondary" -> "#294066",
      "brand-tertiary" -> "#2d5799",
      "gray-dark" -> "#49494B",
      "gray" -> "#7B7B7E",
      "gray-light" -> "#E5E5E6",
      "gray-lighter" -> "#F4F3F4",
      "white-color" -> "#FFFFFF"
    ),
    fork := true,
    scalacOptions --= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Ywarn-unused:imports",
      "-Xlint:-missing-interpolator,_"
    ),
    micrositePushSiteWith := GitHub4s,
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
    micrositeConfigYaml := ConfigYml(
      yamlCustomProperties = Map(
        "epimetheusVersion" -> version.value,
        "catsVersion"       -> catsV,
        "catsEffectVersion" -> catsEffectV,
        "shapelessVersion"  -> shapelessV,
        "scalaVersion"      -> scalaVersion.value,
        "scalaVersions"     -> crossScalaVersions.value.flatMap(CrossVersion.partialVersion).map(_._2).mkString("2.", "/", "") // 2.11/12
      )
    ),
    micrositeExtraMdFiles := Map(
        file("CHANGELOG.md")        -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "100")),
        file("CODE_OF_CONDUCT.md")  -> ExtraMdFileConfig("code-of-conduct.md",   "page", Map("title" -> "code of conduct",   "section" -> "code of conduct",   "position" -> "101")),
        file("LICENSE")             -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "102"))
    )
  )
}

lazy val skipOnPublishSettings = Seq(
  publish / skip := true,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false,
  publishTo := None
)
