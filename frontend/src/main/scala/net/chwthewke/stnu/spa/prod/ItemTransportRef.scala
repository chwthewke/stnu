package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyVector
import cats.syntax.all.*

import data.Countable
import model.Item
import model.prod.FlowEnd
import protocol.persistence.ProcessSplitId

case class ItemTransportRef( ends: Map[FlowEnd, NonEmptyVector[ProcessSplitId]] ):

  def peers(
      item: Item,
      index: Int,
      splitsById: Map[ProcessSplitId, Split[SrcDest]],
      transportSplitRefs: Vector[TransportSplit]
  ): (
      Vector[Countable[Double, Split[SrcDest.Src]]],  // source splits
      Vector[( Double, Int )],                        // source transport splits
      Vector[Countable[Double, Split[SrcDest.Dest]]], // destination splits
      Vector[( Double, Int )]                         // destination transport splits
  ) =
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
        .foldMap( _.toVector )
        .mapFilter: splitId =>
          for
            split               <- splitsById.get( splitId )
            ( amount, srcDest ) <- toSrcDest( split.original, split.end )
          yield Countable( split.copy( original = srcDest ), split.fraction * amount )

    val sourceFlows: Vector[Countable[Double, Split[SrcDest.Src]]] =
      endFlows( FlowEnd.Source )( srcOf )

    val destinationFlows: Vector[Countable[Double, Split[SrcDest.Dest]]] =
      endFlows( FlowEnd.Destination )( destOf )

    val ( transportSplitSources, transportSplitDestinations ) =
      transportSplitRefs
        .foldMap:
          case TransportSplit( amount, from, to ) =>
            (
              Option.when( to == index )( ( amount, from ) ).toVector,
              Option.when( from == index )( ( amount, to ) ).toVector
            )

    ( sourceFlows, transportSplitSources, destinationFlows, transportSplitDestinations )
