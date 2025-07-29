import mill._
import scalalib._

trait CoupledL2VerificationModule extends ScalaModule {

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  def coupledL2Module: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule, utilityModule, coupledL2Module)
}
