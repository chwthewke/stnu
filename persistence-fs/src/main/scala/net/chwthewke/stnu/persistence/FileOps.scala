package net.chwthewke.stnu
package persistence

import cats.Monad
import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.all.*
import fs2.Pipe
import fs2.Stream
import fs2.io.file.FileAlreadyExistsException
import fs2.io.file.FileHandle
import fs2.io.file.Files
import fs2.io.file.Flags
import fs2.io.file.Path
import scodec.Codec
import scodec.stream.StreamDecoder
import scodec.stream.StreamEncoder

object FileOps:

  def listFiles[F[_]: Sync]( dir: Path )( using files: Files[F] ): Stream[F, Path] =
    files.list( dir ).evalFilter( files.isRegularFile )

  def listDirs[F[_]: Sync]( dir: Path )( using files: Files[F] ): Stream[F, Path] =
    files.list( dir ).evalFilter( files.isDirectory )

  def isRegularFile[F[_]]( path: Path )( using files: Files[F] ): F[Boolean] =
    files.isRegularFile( path )

  def isDirectory[F[_]]( path: Path )( using files: Files[F] ): F[Boolean] =
    files.isDirectory( path )

  def createDirectory[F[_]: Sync]( path: Path )( using files: Files[F] ): F[Unit] =
    files.createDirectory( path )

  def createDirectories[F[_]: Sync]( path: Path )( using files: Files[F] ): F[Unit] =
    files.createDirectories( path )

  def deleteDirectory[F[_]: Sync]( path: Path, force: Boolean )( using files: Files[F] ): F[Unit] =
    if ( force ) files.deleteRecursively( path )
    else files.delete( path ).attempt.void

  def touch[F[_]: Sync]( path: Path )( using files: Files[F] ): F[Unit] =
    files
      .createFile( path )
      .attemptNarrow[FileAlreadyExistsException]
      .void

  def readValue[F[_]: Sync, A]( path: Path, codec: Codec[A] )( using files: Files[F] ): F[A] =
    files.readAll( path ).through( read( codec ) ).head.compile.lastOrError

  private def read[F[_]: Sync, A]( codec: Codec[A] ): Pipe[F, Byte, A] =
    StreamDecoder.once( codec ).toPipeByte[F]

  def lock[F[_]: Monad]( path: Path )( using files: Files[F] ): Resource[F, FileHandle[F]] =
    files
      .open( path, Flags.Write )
      .flatTap( handle => Resource.make( handle.lock )( handle.unlock ) )

  private def writeToHandle[F[_]: Sync]( handle: FileHandle[F] )( using files: Files[F] ): Pipe[F, Byte, Nothing] =
    stream =>
      Stream
        .eval( files.writeCursorFromFileHandle( handle, append = false ) )
        .flatMap: cur =>
          cur.writeAll( stream ).void.stream

  def writeValue[F[_]: Sync, A]( handle: FileHandle[F], codec: Codec[A] )( value: A )( using
      files: Files[F]
  ): F[Unit] =
    Stream.emit( value ).through( write( handle, codec ) ).compile.drain

  private def write[F[_]: Sync, A]( handle: FileHandle[F], codec: Codec[A] )( using
      files: Files[F]
  ): Pipe[F, A, Nothing] =
    StreamEncoder.once( codec ).toPipeByte[F].andThen( writeToHandle( handle ) )
