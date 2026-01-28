package net.chwthewke.stnu
package spa
package library

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd

import protocol.persistence.PlanSummary
import spa.library.LibraryMsg.PlanDeleted

case class LibraryModel(
    env: Env,
    plans: Option[Vector[PlanSummary]],
    confirmDeletePlan: Option[PlanSummary]
):
  self =>

  def update[F[_]: Async]( http: Http[F], msg: LibraryMsg ): ( LibraryModel, Cmd[F, LibraryMsg] ) =
    msg match
      case LibraryMsg.LoadLibrary              => self                         -> LibraryModel.loadLibrary( http )
      case LibraryMsg.LibraryLoaded( content ) => copy( plans = content.some ) -> Cmd.None
      case LibraryMsg.RequestDeletePlan( plan ) => copy( confirmDeletePlan = plan.some ) -> Cmd.None
      case LibraryMsg.CloseDeletePlan           => copy( confirmDeletePlan = none )      -> Cmd.None
      case LibraryMsg.ConfirmDeletePlan( planId ) => self -> http.deletePlan( planId ).map( PlanDeleted( _ ) )
      case LibraryMsg.PlanDeleted( false ) => copy( confirmDeletePlan = none ) -> Cmd.None
      case LibraryMsg.PlanDeleted( true )  => copy( confirmDeletePlan = none ) -> LibraryModel.loadLibrary( http )

object LibraryModel:
  def init( env: Env ): LibraryModel = LibraryModel( env, None, None )

  def loadLibrary[F[_]: Async]( http: Http[F] ): Cmd[F, LibraryMsg] =
    http.loadLibrary.map( LibraryMsg.LibraryLoaded( _ ) )
