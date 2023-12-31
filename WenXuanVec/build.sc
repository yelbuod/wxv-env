// import Mill dependency
import mill._
import mill.scalalib._
import mill.scalalib.TestModule.Utest
// support BSP
import mill.bsp._

object difftest extends SbtModule {
  override def millSourcePath = os.pwd / "difftest"
  override def scalaVersion = "2.12.13"
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.4.3",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.4.3",
    ivy"org.scalamacros:::paradise:2.1.1"
  )
}

object wenxuanvec extends SbtModule { m =>
  override def millSourcePath = os.pwd
  override def scalaVersion = "2.12.13"
  override def scalacOptions = Seq(
    "-Xsource:2.11",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    // Enables autoclonetype2 in 3.4.x (on by default in 3.5)
    "-P:chiselplugin:useBundlePlugin"
  )
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.4.3",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.4.3",
    ivy"org.scalamacros:::paradise:2.1.1"
  )
  object test extends Tests with Utest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::utest:0.7.10",
      ivy"edu.berkeley.cs::chiseltest:0.3.3",
    )
  }
  override def moduleDeps = super.moduleDeps ++ Seq(
    difftest
  )
}
