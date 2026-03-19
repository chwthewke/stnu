package net.chwthewke.stnu
package persistence

import fs2.io.file.Path
import pureconfig.ConfigReader

private given ConfigReader[Path] = ConfigReader[String].map( str => Path( str ) )

case class FsConfig( dataDir: Path ) derives ConfigReader
