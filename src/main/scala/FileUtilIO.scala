package io.nineclue.filemover

import cats.effect._
import cats.implicits._
import scala.util.Try
import java.io.File
import java.nio.file.{Path, Paths, Files, AccessDeniedException}
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.StreamConverters._

object FileUtilIO {
    def makeFolder(fname: String, tbase: Option[Path] = None): IO[Unit] = {
        def job = {
            val path = tbase match {
                case Some(tb) => 
                    tb.resolve(fname)
                case None =>
                    Paths.get(fname)
            }
            Files.createDirectories(path)
        }
        IO(job).handleErrorWith(_ match {
            case e: AccessDeniedException => 
                IO(println(s"Error: $e")) *>
                IO.raiseError(e)
            case _ =>
                IO(())
        }).void
    }

    def listFiles(path: Path, recursive: Boolean = true, ignoreHidden: Boolean = true): IO[List[Path]] = {
        val level = if (recursive) Int.MaxValue else 1
        def filter(p: Path, fa: BasicFileAttributes) = 
            fa.isRegularFile() && 
            (!ignoreHidden || p.getFileName.toString.headOption.map(_ != '.').getOrElse(false))
        IO(Files.find(path, level, filter).toScala(List))
    }

    def oPath(p: String): IO[Option[Path]] = {
        val path = Paths.get(p).toAbsolutePath()
        IO(Option.when(Files.exists(path) && Files.isDirectory(path))(path))
    }

    def moveTo(f: Path, tgt: Path, fa: FileAttributes[_]): IO[Unit] = {
        val tDir = fa.apply(f)
        for {
            _ <- makeFolder(tDir, Some(tgt))
            tpath = tgt.resolve(tDir) // append target directory
            finalPath = tpath.resolve(f.getFileName()) // append file name
            _ <- IO(println(s"Moving $f to $finalPath"))
            _ <- IO(Files.move(f, finalPath))    // actual move
        } yield()
    }
}