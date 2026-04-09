package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyVector
import cats.syntax.all.*
import mouse.option.*

import data.Countable
import model.Item
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.ProcessSplitId

case class ItemTransportRef( ends: Map[FlowEnd, NonEmptyVector[ProcessSplitId]] ):
  def toItemTransport(
      prod: ProdModel,
      item: Item,
      index: Int,
      endsBySplitId: Map[ProcessSplitId, ( Double, Group, EndId )],
      endSplits: Map[EndId, ProcessSplits],
      splitsById: Map[ProcessSplitId, Split[SrcDest]],
      transportSplitRefs: Vector[TransportSplit]
  ): ItemTransport =

    def srcOf( srcDest: SrcDest, endId: EndId ): Option[( Double, SrcDest.Src )] =
      def srcAmount( process: ClockedRecipe ): Double =
        process.productsPerMinute.find( _.item.className == item.className ).foldMap( _.amount )
      srcDest match
        case s @ SrcDest.Extract( process ) => ( srcAmount( process ), s ).some
        case s @ SrcDest.Step( process )    => ( srcAmount( process ), s ).some
        case SrcDest.Input                  =>
          endId match
            case EndId.Input( ci ) => ( ci.amount, SrcDest.Input ).some
            case _                 => none
        case _ => none

    def destOf( srcDest: SrcDest, endId: EndId ): Option[( Double, SrcDest.Dest )] =
      def destAmount( process: ClockedRecipe ): Double =
        process.ingredientsPerMinute.find( _.item.className == item.className ).foldMap( _.amount )
      srcDest match
        case s @ SrcDest.Step( process )           => ( destAmount( process ), s ).some
        case SrcDest.Requested | SrcDest.Byproduct =>
          endId match
            case EndId.Requested( ci ) => ( ci.amount, SrcDest.Requested ).some
            case EndId.Byproduct( ci ) => ( ci.amount, SrcDest.Byproduct ).some
            case _                     => none
        case _ => none

    def endFlows[A <: SrcDest](
        end: FlowEnd
    )( toSrcDest: ( SrcDest, EndId ) => Option[( Double, A )] ): Vector[Countable[Double, Split[A]]] =
      ends
        .get( end )
        .cata( _.toVector, Vector.empty )
        .mapFilter: splitId =>
          for
            split               <- splitsById.get( splitId )
            ( amount, srcDest ) <- toSrcDest( split.original, split.end )
          yield Countable( split.copy( original = srcDest ), split.fraction * amount )

    val sourceFlows: Vector[Countable[Double, Split[SrcDest.Src]]] =
      endFlows( FlowEnd.Source )( srcOf )

    val destinationFlows: Vector[Countable[Double, Split[SrcDest.Dest]]] =
      endFlows( FlowEnd.Destination )( destOf )

    val transportSplits: Map[FlowEnd, Vector[Countable[Double, Int]]] =
      transportSplitRefs
        .foldMap:
          case TransportSplit( amount, from, to ) =>
            Option.when( from == index )( Map( FlowEnd.destination -> Vector( Countable( to, amount ) ) ) )
              |+| Option.when( to == index )( Map( FlowEnd.source -> Vector( Countable( from, amount ) ) ) )
        .orEmpty

    ItemTransport( prod, item, sourceFlows, destinationFlows, transportSplits )
