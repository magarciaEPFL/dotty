package dotty.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import typer.{FrontEnd, Typer, Mode, ImportInfo}
import reporting.ConsoleReporter

class Compiler {

  def phases = List(new FrontEnd, new backend.jvm.GenBCode.BCodePhase)

  def rootContext(implicit ctx: Context): Context = {
    ctx.definitions.init()
    ctx.usePhases(phases)
    val start = ctx.fresh
      .withPeriod(Period(ctx.runId + 1, FirstPhaseId))
      .withOwner(defn.RootClass)
      .withTyper(new Typer)
      .withNewMode(Mode.ImplicitsEnabled)
      .withTyperState(new MutableTyperState(ctx.typerState, new ConsoleReporter()(ctx), isCommittable = true))
    def addImport(ctx: Context, sym: Symbol) =
      ctx.fresh.withImportInfo(ImportInfo.rootImport(sym)(ctx))
    (start.withRunInfo(new RunInfo(start)) /: defn.RootImports)(addImport)
  }

  def newRun(implicit ctx: Context): Run =
    new Run(this)(rootContext)
}
