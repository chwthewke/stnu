package net.chwthewke.stnu
package spa

import model.ModelIndex
import protocol.game.FullModel
import spa.browse.BrowseMsg

enum Msg:
  case Noop
  case FetchGameModel( version: ModelVersionId )
  case RecvGameModel( index: ModelIndex, model: FullModel )
  case SetLocation( location: LocationModel )
  case NavigateExternal( href: String )
  case BrowseMessage( payload: BrowseMsg )
