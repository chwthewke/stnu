package net.chwthewke.stnu
package spa
package library

import protocol.persistence.PlanId
import protocol.persistence.PlanSummary

enum LibraryMsg:
  case LoadLibrary
  case LibraryLoaded( content: Vector[PlanSummary] )
  case RequestDeletePlan( planId: PlanSummary )
  case CloseDeletePlan
  case ConfirmDeletePlan( planId: PlanId )
  case PlanDeleted( deleted: Boolean )
