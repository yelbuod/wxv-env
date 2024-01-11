// import Mill dependency
import mill._
import mill.scalalib._
// import scalalib._
// import mill.scalalib.TestModule.Utest
// support BSP
// import mill.bsp._
import $file.XiangShan.`rocket-chip`.common
import $file.XiangShan.`rocket-chip`.cde.common
import $file.XiangShan.`rocket-chip`.hardfloat.build
import $file.XiangShan.huancun.common
import $file.XiangShan.coupledL2.common
import $file.XiangShan.common

// object difftest extends SbtModule {
//   override def millSourcePath = os.pwd / "difftest"
//   override def scalaVersion = "2.12.13"
//   override def ivyDeps = Agg(
//     ivy"edu.berkeley.cs::chisel3:3.4.3",
//   )
//   override def scalacPluginIvyDeps = Agg(
//     ivy"edu.berkeley.cs:::chisel3-plugin:3.4.3",
//     ivy"org.scalamacros:::paradise:2.1.1"
//   )
// }

val defaultScalaVersion = "2.13.10"

def defaultVersions(chiselVersion: String) = chiselVersion match {
  case "chisel" => Map(
    "chisel"        -> ivy"org.chipsalliance::chisel:6.0.0-RC1",
    "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.0.0-RC1",
    "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:5.0.2"
  )
  case "chisel3" => Map(
    "chisel"        -> ivy"edu.berkeley.cs::chisel3:3.6.0",
    "chisel-plugin" -> ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0",
    "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:0.6.2"
  )
}

trait HasChisel extends SbtModule with Cross.Module[String] {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends Cross[RocketChip]("chisel", "chisel3")

trait RocketChip
  extends millbuild.XiangShan.`rocket-chip`.common.RocketChipModule
    with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "XiangShan" / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat(crossValue)

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"

  object macros extends Macros

  trait Macros
    extends millbuild.XiangShan.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat extends Cross[Hardfloat](crossValue)

  trait Hardfloat
    extends millbuild.XiangShan.`rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "XiangShan" / "rocket-chip" / "hardfloat" / "hardfloat"

  }

  object cde extends CDE

  trait CDE extends millbuild.XiangShan.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "XiangShan" / "rocket-chip" / "cde" / "cde"
  }
}

object utility extends Cross[Utility]("chisel", "chisel3")
trait Utility extends HasChisel {

  override def millSourcePath = os.pwd / "XiangShan" / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip(crossValue)
  )

}

object huancun extends Cross[HuanCun]("chisel", "chisel3")
trait HuanCun extends millbuild.XiangShan.huancun.common.HuanCunModule with HasChisel {

  override def millSourcePath = os.pwd / "XiangShan" / "huancun"

  def rocketModule: ScalaModule = rocketchip(crossValue)

  def utilityModule: ScalaModule = utility(crossValue)

}

object coupledL2 extends Cross[CoupledL2]("chisel", "chisel3")
trait CoupledL2 extends millbuild.XiangShan.coupledL2.common.CoupledL2Module with HasChisel {

  override def millSourcePath = os.pwd / "XiangShan" / "coupledL2"

  def rocketModule: ScalaModule = rocketchip(crossValue)

  def utilityModule: ScalaModule = utility(crossValue)

  def huancunModule: ScalaModule = huancun(crossValue)

}

object difftest extends Cross[Difftest]("chisel", "chisel3")
trait Difftest extends HasChisel {

  override def millSourcePath = os.pwd / "XiangShan" / "difftest"

}

object fudian extends Cross[FuDian]("chisel", "chisel3")
trait FuDian extends HasChisel {

  override def millSourcePath = os.pwd / "XiangShan" / "fudian"

}

object xiangshan extends Cross[XiangShan]("chisel", "chisel3")
trait XiangShan extends millbuild.XiangShan.common.XiangShanModule with HasChisel {

  override def millSourcePath = os.pwd / "XiangShan"

  def rocketModule = rocketchip(crossValue)

  def difftestModule = difftest(crossValue)

  def huancunModule = huancun(crossValue)

  def coupledL2Module = coupledL2(crossValue)

  def fudianModule = fudian(crossValue)

  def utilityModule = utility(crossValue)

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(millSourcePath / "src" / crossValue / "main" / "scala"))
  }

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xmx20G", "-Xss256m")

    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(millSourcePath / "src" / crossValue / "test" / "scala"))
    }

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}

// object xiangshan extends Cross[millbuild.XiangShan.build.XiangShanModule]("chisel", "chisel3") {
//   override def millSourcePath = os.pwd / "XiangShan"
// }

trait WenXuanVecModule extends ScalaModule {
  def xiangshanModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    xiangshanModule
  )
}


object wenxuanvec extends Cross[WenXuanVec]("chisel", "chisel3")
trait WenXuanVec extends WenXuanVecModule with HasChisel {
  
  override def millSourcePath = os.pwd

  def xiangshanModule = xiangshan(crossValue)

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}

// object wenxuanvec extends SbtModule { m =>
//   override def millSourcePath = os.pwd
//   override def scalaVersion = "2.12.13"
//   override def scalacOptions = Seq(
//     "-Xsource:2.11",
//     "-language:reflectiveCalls",
//     "-deprecation",
//     "-feature",
//     "-Xcheckinit",
//     // Enables autoclonetype2 in 3.4.x (on by default in 3.5)
//     "-P:chiselplugin:useBundlePlugin"
//   )
//   override def ivyDeps = Agg(
//     ivy"edu.berkeley.cs::chisel3:3.4.3",
//   )
//   override def scalacPluginIvyDeps = Agg(
//     ivy"edu.berkeley.cs:::chisel3-plugin:3.4.3",
//     ivy"org.scalamacros:::paradise:2.1.1"
//   )
//   object test extends Tests with Utest {
//     override def ivyDeps = m.ivyDeps() ++ Agg(
//       ivy"com.lihaoyi::utest:0.7.10",
//       ivy"edu.berkeley.cs::chiseltest:0.3.3",
//     )
//   }

//   def xiangshanModule = xiangshan("chisel")

//   override def moduleDeps = super.moduleDeps ++ Seq(
//     difftest,
//     xiangshanModule
//   )
// }
