import mill._, scalalib._, scalafmt._

object lib extends ScalaModule {
  def scalaVersion = "2.13.7"
}

trait Common extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.7"

  override def moduleDeps: Seq[JavaModule] = Seq(lib)
}

object nineteen extends ScalaModule with Common {
  object one extends ScalaModule with Common
  object two extends ScalaModule with Common
  object three extends ScalaModule with Common
  object four extends ScalaModule with Common
  object five extends ScalaModule with Common
  object six extends ScalaModule with Common
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
}
