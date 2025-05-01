package net.chwthewke.stnu
package server
package api

import cats.data.OptionT
import cats.~>

import model.Model
import model.ModelIndex

trait ModelApi[F[_]]:
  self =>

  def getModelIndex: F[ModelIndex]

  def getModel( version: Int ): OptionT[F, Model]

  def getLatestModel: F[Model]

  final def mapK[G[_]]( f: F ~> G ): ModelApi[G] = new ModelApi[G]:
    override def getModelIndex: G[ModelIndex] =
      f( self.getModelIndex )

    override def getModel( version: Int ): OptionT[G, Model] =
      self.getModel( version ).mapK( f )

    override def getLatestModel: G[Model] =
      f( self.getLatestModel )
