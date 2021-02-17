package io.nineclue.filemover

import cats.effect._
import cats.implicits._
import scala.util.Try
import java.io.File
import java.nio.file.{Path, Paths, Files, AccessDeniedException}
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.StreamConverters._

object FileMover extends IOApp {
    def makeFolder(fname: String): IO[Unit] = {
        def job = {
            val path = Paths.get(fname)
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

    def files(path: String, recursive: Boolean = true, ignoreHidden: Boolean = true): IO[List[Path]] = {
        val level = if (recursive) Int.MaxValue else 1
        def filter(p: Path, fa: BasicFileAttributes) = 
            fa.isRegularFile() && 
            (!ignoreHidden || p.getFileName.toString.headOption.map(_ != '.').getOrElse(false))
        IO(Files.find(Paths.get(path), level, filter).toScala(List))
    }

    def moveTo(path: Path, fa: FileAttributes[_]) = {
        val tDir = fa.apply(path)
        for {
            _ <- makeFolder(tDir)
            _ <- IO(println(tDir))
            _ <- IO(Files.move())
        } yield()
    }
    

    def run(args: List[String]): IO[ExitCode] = {
        val srcName = args.headOption.getOrElse(".")
        val tgtName = args.drop(1).headOption.getOrElse("folder")
        val absSrc = Paths.get(srcName).toAbsolutePath()
        val absTgt = Paths.get(tgtName).toAbsolutePath()
        val p = 
            if (Files.exists(absSrc) && Files.isDirectory(absSrc))
                IO.raiseError(new Exception("invalid Source path."))
            else {
                for {
                    _ <- IO(println(s"$absSrc -> $tgtName"))
                    ps <- files(srcName, false)
                    _ <- ps.traverse(p => IO(println(s"$srcName : ${p.getFileName()} => ${FileNameAttributes(p)}")))
                } yield()
            } 
        p.as(ExitCode.Success)
    }
}