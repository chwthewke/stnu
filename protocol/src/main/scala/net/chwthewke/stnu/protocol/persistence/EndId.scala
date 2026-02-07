package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.Item
import model.Recipe

enum EndId derives ConfiguredCodec:
  case Process( recipe: ClassName[Recipe] )
  case Input( item: ClassName[Item] )
  case Requested( item: ClassName[Item] )
  case Byproduct( item: ClassName[Item] )
