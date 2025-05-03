package net.chwthewke.stnu
package protocol
package game

import cats.data.OptionT
import cats.~>

import model.Model
import model.ModelIndex
import protocol.codec.PathCodec
import protocol.codec.SegmentCodec
import protocol.codec.UriCodec

trait ModelApi[F[_]]:
  self =>

  def getModelIndex: F[ModelIndex]

  def getModel( version: ModelVersionId ): OptionT[F, Model]

  def getLatestModel: F[Model]

  final def mapK[G[_]]( f: F ~> G ): ModelApi[G] = new ModelApi[G]:
    override def getModelIndex: G[ModelIndex] =
      f( self.getModelIndex )

    override def getModel( version: ModelVersionId ): OptionT[G, Model] =
      self.getModel( version ).mapK( f )

    override def getLatestModel: G[Model] =
      f( self.getLatestModel )

object ModelApi:

  import PathCodec.Root
  import SegmentCodec.*

  val getModelIndex: UriCodec.Constant   = Root / "api" / "models"
  val getModel: UriCodec[ModelVersionId] = Root / "api" / "model" / Int.imap( ModelVersionId( _ ), _.id )
  val getLatestModel: UriCodec.Constant  = Root / "api" / "model" / "latest"
