package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

import model.ClockSpeedPreset
import model.Machine

case class PowerOptions(
    allowedGenerators: Set[ClassName[Machine]],
    maxProductionBoostInput: InputModel,
    maxProductionBoost: Int,
    manufacturingClockSpeed: ClockSpeedPreset
):
  def restore: PowerOptions = copy( maxProductionBoostInput = maxProductionBoostInput.restore )

  def setOption( env: Env, option: PowerOption ): PowerOptions =
    option match
      case PowerOption.Reset =>
        copy( allowedGenerators = env.powerGenerators.foldMap( m => Set( m.className ) ) )
      case PowerOption.SetPowerGenerator( generator, enable ) =>
        val newGenerators: Set[ClassName[Machine]] =
          if ( enable )
            allowedGenerators ++ env.powerGenerators.find( _.className == generator ).map( _.className )
          else
            allowedGenerators - generator
        copy( allowedGenerators = newGenerators )
      case PowerOption.SetMaxProductionBoost( value ) =>
        value.toIntOption.fold( this )( b => copy( maxProductionBoost = b ) )
      case PowerOption.SetManufacturingClockSpeed( csp ) =>
        copy( manufacturingClockSpeed = csp )

object PowerOptions:
  def init( env: Env ): PowerOptions =
    PowerOptions(
      env.powerGenerators.map( _.className ).toSet,
      InputModel.withDefault( "0" ),
      0,
      ClockSpeedPreset.`100%`
    )

  given Conversion[PowerOptions, pp.PowerOptions]:
    override def apply( x: PowerOptions ): pp.PowerOptions =
      pp.PowerOptions( x.allowedGenerators, x.maxProductionBoost, x.manufacturingClockSpeed )

  def from( p: pp.PowerOptions ): PowerOptions =
    PowerOptions(
      p.allowedGenerators,
      InputModel.withDefault( p.maxProductionBoost.toString ),
      p.maxProductionBoost,
      p.manufactutingClockSpeed
    )
