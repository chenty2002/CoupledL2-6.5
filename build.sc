import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build


trait HasChisel6 extends ScalaModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(ivy"org.chipsalliance::chisel:6.6.0")

  def chiselPluginIvy: Option[Dep] = Some(ivy"org.chipsalliance:::chisel-plugin:6.6.0")

  override def scalaVersion = "2.13.15"

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends `rocket-chip`.common.RocketChipModule with HasChisel6 {

  val rcPath = os.pwd / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends `rocket-chip`.common.MacrosModule with HasChisel6 {
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  object cde extends `rocket-chip`.cde.common.CDEModule with HasChisel6 {
    override def millSourcePath = rcPath / "cde" / "cde"
  }

  object hardfloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel6 {
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

}

object utility extends SbtModule with HasChisel6 {
  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
}

object huancun extends SbtModule with HasChisel6 {
  override def millSourcePath = os.pwd / "coupledL2" / "HuanCun"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object coupledL2 extends SbtModule with HasChisel6 {
  override def millSourcePath = os.pwd / "coupledL2"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility, huancun
  )
}

object CoupledL2Assume extends SbtModule with HasChisel6 with millbuild.common.CoupledL2AssumeModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def coupledL2Module: ScalaModule = coupledL2

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:6.0.0",
    )
  }
}
