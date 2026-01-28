package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

import model.Machine

case class PowerOptions(
    allowedGenerators: Set[ClassName[Machine]]
):
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

object PowerOptions:
  def init( env: Env ): PowerOptions =
    PowerOptions( env.powerGenerators.map( _.className ).toSet )

  given Conversion[PowerOptions, pp.PowerOptions]:
    override def apply( x: PowerOptions ): pp.PowerOptions =
      pp.PowerOptions( x.allowedGenerators )

  def from( p: pp.PowerOptions ): PowerOptions =
    PowerOptions( p.allowedGenerators )
