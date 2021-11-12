import mill._, scalalib._, scalafmt._

trait Common extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.7"

  override def moduleDeps: Seq[JavaModule] = Seq(lib)
}

object lib extends ScalaModule {
  def scalaVersion = "2.13.7"
}

object nineteen extends ScalaModule with Common {
  object one extends ScalaModule with Common
}

