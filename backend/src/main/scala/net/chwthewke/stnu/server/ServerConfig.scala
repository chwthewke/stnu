package net.chwthewke.stnu
package server

import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import org.http4s.Uri
import pureconfig.ConfigReader
import pureconfig.generic.semiauto.deriveReader
import pureconfig.module.http4s.*
import pureconfig.module.ip4s.*

private given Show[Ipv4Address] = Show[Host].narrow

case class ServerConfig(
    listenAddress: Ipv4Address,
    listenPort: Port,
    baseUri: Uri
) derives Show:
  def frontendFlags: Map[String, String] = Map( "backend" -> baseUri.renderString )

object ServerConfig:
  given ConfigReader[ServerConfig] = deriveReader[ServerConfig]
