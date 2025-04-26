package net.chwthewke.stnu
package ingest

import cats.Show
import cats.derived.strict.*
import fs2.io.file.Path
import scodec.bits.ByteVector

private given Show[ByteVector] = Show.show( _.toHex )

case class TextureData( name: String, path: Path, hash: ByteVector ) derives Show:
  def filename: String = s"${hash.toHex}.png"
