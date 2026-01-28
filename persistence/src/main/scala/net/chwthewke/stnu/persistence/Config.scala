package net.chwthewke.stnu
package persistence

import pureconfig.ConfigReader

case class Config( databaseName: String, user: String, password: String ) derives ConfigReader
