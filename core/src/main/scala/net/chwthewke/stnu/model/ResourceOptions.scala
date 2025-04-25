package net.chwthewke.stnu
package model

import algebra.lattice.MeetSemilattice
import cats.Eq
import cats.Show
import cats.data.Ior
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class ResourceOptions(
    resourceNodes: Map[ExtractorType, Map[ClassName, ResourceDistrib]],
    resourceWeights: ResourceWeights
) derives ConfiguredDecoder,
      ConfiguredEncoder:
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractor.flatMap( resourceNodes.get ).flatMap( _.get( item.className ) ).orEmpty

  def mergeResourceNodes( defaultResourceNodes: Map[ExtractorType, Map[ClassName, ResourceDistrib]] ): ResourceOptions =
    copy(resourceNodes =
      resourceNodes
        .alignMergeWith( defaultResourceNodes )(
          _.alignWith( _ ):
            case Ior.Left( a )    => a
            case Ior.Both( a, b ) => MeetSemilattice.meet( a, b )
            case Ior.Right( b )   => b
        )
    )

object ResourceOptions:

  val empty: ResourceOptions = ResourceOptions( Map.empty, ResourceWeights( Map.empty ) )

  given Show[ResourceOptions] =
    def showItem( item: ClassName, distrib: ResourceDistrib ): String =
      show"${item.name.padTo( 32, ' ' )} P ${f"${distrib.pureNodes}% 2d"} " +
        show"N ${f"${distrib.normalNodes}% 2d"} I ${f"${distrib.impureNodes}% 2d"}"

    def showExtractorType( extractorType: ExtractorType, items: Map[ClassName, ResourceDistrib] ): String =
      items.toVector.map( showItem.tupled ).mkString_( show"${extractorType.description}\n  ", "\n  ", "" )

    Show.show: opts =>
      show"""NODES
            |${opts.resourceNodes.toVector.map( showExtractorType.tupled ).mkString_( "\n\n" )}
            |
            |WEIGHTS
            |${opts.resourceWeights}
            |""".stripMargin

  given Eq[ResourceOptions] = Eq.by( ro => ( ro.resourceNodes, ro.resourceWeights ) )
