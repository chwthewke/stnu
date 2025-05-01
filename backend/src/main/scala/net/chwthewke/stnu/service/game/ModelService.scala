package net.chwthewke.stnu
package service
package game

import cats.Applicative
import cats.data.NonEmptyMap
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all.*

import model.Model
import model.ModelIndex
import server.api.ModelApi

class ModelService[F[_]: Applicative](
    private val index: ModelIndex,
    private val models: NonEmptyMap[Int, Model]
) extends ModelApi[F]:
  def getModelIndex: F[ModelIndex] =
    index.pure[F]

  def getModel( version: Int ): OptionT[F, Model] =
    OptionT.fromOption[F]( models.get( version ) )

  def getLatestModel: F[Model] = models.last._2.pure[F]

object ModelService:
  def load[F[_]: Sync]: F[ModelService[F]] =
    for
      modelIndex <- assets.loadModelIndex[F]
      models <- modelIndex.versions.toVector.traverse: version =>
                  assets.loadModel[F]( version ).tupleLeft( version.version )
      modelsNev <- models.toNev.liftTo[F]( Error( "No model was loaded - index empty" ) )
    yield new ModelService[F]( modelIndex, modelsNev.toNem )
