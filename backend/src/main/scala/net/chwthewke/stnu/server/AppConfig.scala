package net.chwthewke.stnu
package server

import pureconfig.ConfigReader
import pureconfig.generic.semiauto.deriveReader

import server.middleware.LoggingMiddleware

case class AppConfig(
    server: ServerConfig,
    logging: LoggingMiddleware.Config
)

object AppConfig:
  given ConfigReader[AppConfig] = deriveReader[AppConfig]
