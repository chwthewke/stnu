package net.chwthewke.stnu
package spa
package prod

import munit.FunSuite

class EnvTests extends FunSuite with GameFixture:
  test( "read env - r1.1" ):
    val env = `r1.1`
    assert( clue( env.game.items.size ) == 169 )
    assert( clue( env.game.recipes.size ) == 412 )
