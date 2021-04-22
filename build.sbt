lazy val sokoban_editor = (project in file("."))
    .settings(
        name := "sokoban_editor",
        version := "1.0",
        scalaVersion := "2.12.7"
    )
    .aggregate(planner)
    .dependsOn(planner)

lazy val planner = RootProject(file("./planner/"))
