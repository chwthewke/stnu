package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd
import tyrian.cmds.Dom

import protocol.persistence.PlanId
import protocol.persistence.PlanName

case class PlanNameModel(
    name: PlanName,
    saved: Option[( PlanId, pp.Plan )],
    input: Option[InputModel],
    confirmSaving: Boolean,
    confirmRevert: Boolean,
    confirmNew: Boolean
):
  def duplicate: PlanNameModel =
    copy(
      name = PlanName( name.name + " (copy)" ),
      saved = none
    )

  def restore: PlanNameModel = copy( input = input.map( _.restore ) )

  def startEdit[F[_]: Async]: ( PlanNameModel, Cmd[F, Nothing] ) =
    copy( input = InputModel.withDefault( name.name ).some ) -> Dom.focus( PlanNameModel.editorId )

  def cancelEdit: PlanNameModel =
    copy( input = none )

  def commitEdit: PlanNameModel =
    input.foldLeft( this ): ( current, edited ) =>
      edited.input
        .filter( _.nonEmpty )
        .fold( current.copy( input = none ) )( nextName =>
          current.copy( input = none, name = PlanName( nextName ), saved = none )
        )

  def update[F[_]: Async]( http: Http[F], action: PlanNameAction ): ( PlanNameModel, Cmd[F, Nothing] ) =
    action match
      case PlanNameAction.EditStart             => startEdit
      case PlanNameAction.EditCancel            => cancelEdit -> Cmd.None
      case PlanNameAction.EditCommit            => commitEdit -> Cmd.None
      case PlanNameAction.EditSetValue( value ) =>
        copy( input = input.as( InputModel.onInput( value ) ) ) -> Cmd.None
      case PlanNameAction.SaveResponse( Some( id ), saved ) =>
        copy( confirmSaving = false, saved = ( id, saved ).some ) -> Cmd.None
      case PlanNameAction.SaveResponse( None, _ ) =>
        copy( confirmSaving = true ) -> Cmd.None
      case PlanNameAction.SaveCancel =>
        copy( confirmSaving = false ) -> Cmd.None
      case PlanNameAction.Duplicate =>
        duplicate -> Cmd.None
      case PlanNameAction.Revert =>
        copy( confirmRevert = true ) -> Cmd.None
      case PlanNameAction.RevertCancel =>
        copy( confirmRevert = false ) -> Cmd.None
      case PlanNameAction.Clear =>
        copy( confirmNew = true ) -> Cmd.None
      case PlanNameAction.ClearCancel =>
        copy( confirmNew = false ) -> Cmd.None

object PlanNameModel:
  def init( name: PlanName, saved: Option[( PlanId, pp.Plan )] ): PlanNameModel =
    PlanNameModel( name, saved, none, false, false, false )

  val editorId: String = "plan_name_editor"
