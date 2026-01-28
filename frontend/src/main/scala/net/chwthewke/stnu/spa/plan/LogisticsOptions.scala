package net.chwthewke.stnu
package spa
package plan

import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.collection.immutable.SortedSet

import model.Transport

case class LogisticsOptions(
    belt: ClassName[Transport],
    pipeline: ClassName[Transport],
    allBelts: SortedSet[ClassName[Transport]],
    allPipelines: SortedSet[ClassName[Transport]],
    useAll: Boolean
):
  private def selected(
      all: NonEmptyList[Transport],
      multi: Set[ClassName[Transport]],
      single: ClassName[Transport]
  ): NonEmptyList[Transport] =
    all
      .filter( b => if ( useAll ) multi.contains( b.className ) else single == b.className )
      .toNel
      .getOrElse( NonEmptyList.one( all.last ) )

  def belts( env: Env ): NonEmptyList[Transport] =
    selected( env.conveyorBelts, allBelts, belt )
  def pipelines( env: Env ): NonEmptyList[Transport] =
    selected( env.defaultPipelines, allPipelines, pipeline )

  def setUseAll( env: Env ): LogisticsOptions =
    copy(
      useAll = true,
      allBelts = LogisticsOptions.allUpTo( env.conveyorBelts, belt ),
      allPipelines = LogisticsOptions.allUpTo( env.defaultPipelines, pipeline )
    )
  def setUseSingle( env: Env ): LogisticsOptions =
    copy(
      useAll = false,
      belt = LogisticsOptions.bestOf( env.conveyorBelts, allBelts ),
      pipeline = LogisticsOptions.bestOf( env.defaultPipelines, allPipelines )
    )

  def setOption( env: Env, option: LogisticsOption ): LogisticsOptions =
    option match
      case LogisticsOption.SetBelt( belt ) =>
        copy( belt = belt )
      case LogisticsOption.SetPipeline( pipeline ) =>
        copy( pipeline = pipeline )
      case LogisticsOption.ToggleUseAll( enable ) =>
        if ( enable ) setUseAll( env ) else setUseSingle( env )
      case LogisticsOption.SelectBelt( belt, enable ) =>
        copy( allBelts = if ( enable ) allBelts + belt else allBelts - belt )
      case LogisticsOption.SelectPipeline( pipeline, enable ) =>
        copy( allPipelines = if ( enable ) allPipelines + pipeline else allPipelines - pipeline )

object LogisticsOptions:
  private def allUpTo( source: NonEmptyList[Transport], best: ClassName[Transport] ): SortedSet[ClassName[Transport]] =
    source.toList.takeWhile( _.className != best ).map( _.className ).to( SortedSet ) + best

  private def bestOf(
      source: NonEmptyList[Transport],
      selected: SortedSet[ClassName[Transport]]
  ): ClassName[Transport] =
    source.toList.findLast( t => selected( t.className ) ).getOrElse( source.last ).className

  def init( env: Env ): LogisticsOptions =
    LogisticsOptions(
      env.conveyorBelts.last.className,
      env.defaultPipelines.last.className,
      env.conveyorBelts.map( _.className ).toNes.toSortedSet,
      env.defaultPipelines.map( _.className ).toNes.toSortedSet,
      useAll = false
    )

  given Conversion[LogisticsOptions, pp.LogisticsOptions]:
    override def apply( x: LogisticsOptions ): pp.LogisticsOptions =
      pp.LogisticsOptions(
        Option.when( !x.useAll )( x.belt ),
        Option.when( !x.useAll )( x.pipeline ),
        x.allBelts,
        x.allPipelines
      )

  def from( env: Env, p: pp.LogisticsOptions ): LogisticsOptions =
    LogisticsOptions(
      p.singleBelt.getOrElse( bestOf( env.conveyorBelts, p.allBelts ) ),
      p.singlePipeline.getOrElse( bestOf( env.defaultPipelines, p.allPipelines ) ),
      p.allBelts,
      p.allPipelines,
      p.singleBelt.isEmpty || p.singlePipeline.isEmpty
    )
