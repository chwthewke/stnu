package net.chwthewke.stnu
package spa

import org.http4s.Uri

import data.ImageName

abstract class Links:
  def backend: Uri

  def image( imageName: ImageName ): Uri =
    backend / "static" / "img" / imageName.name
