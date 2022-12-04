import mill._, scalalib._, scalafmt._

object lib extends ScalaModule {
  def scalaVersion = "2.13.8"

  def scalacOptions = Seq("-deprecation")
}

trait Common extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.8"

  def scalacOptions = Seq("-deprecation")

  override def moduleDeps: Seq[JavaModule] = Seq(lib)
}

trait Common3 extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.2.0"

  def scalacOptions = Seq("-deprecation")

  override def moduleDeps: Seq[JavaModule] = Seq(lib)
}
trait Json extends ScalaModule {
  def ivyDeps = Agg(
    ivy"com.typesafe.play::play-json:2.9.2",
  )
}

object nineteen extends ScalaModule with Common {
  object one extends ScalaModule with Common
  object two extends ScalaModule with Common
  object three extends ScalaModule with Common
  object four extends ScalaModule with Common
  object five extends ScalaModule with Common
  object six extends ScalaModule with Common
  object seven extends ScalaModule with Common
  object eight extends ScalaModule with Common
}

object twentyone extends ScalaModule with Common {
  object one extends ScalaModule with Common
  object two extends ScalaModule with Common
  object three extends ScalaModule with Common
  object four extends ScalaModule with Common
  object five extends ScalaModule with Common
  object six extends ScalaModule with Common
  object seven extends ScalaModule with Common
  object eight extends ScalaModule with Common
  object nine extends ScalaModule with Common
  object ten extends ScalaModule with Common
  object eleven extends ScalaModule with Common
  object twelve extends ScalaModule with Common
  object thirteen extends ScalaModule with Common
  object fourteen extends ScalaModule with Common
  object fifteen extends ScalaModule with Common
  object sixteen extends ScalaModule with Common
  object seventeen extends ScalaModule with Common
  object eighteen extends ScalaModule with Common with Json
  object twenty extends ScalaModule with Common
  object twentytwo extends ScalaModule with Common
  object twentythree extends ScalaModule with Common
}

object twentytwo extends Module {
  object one extends ScalaModule with Common
  object two extends ScalaModule with Common
  object three extends ScalaModule with Common
  object four extends ScalaModule with Common
}
