import cats.effect._
import cats.implicits._
import scala.util.Random.{nextInt, alphanumeric}
import scala.util.Try
import java.io.File
import java.nio.file.{Paths, Files, AccessDeniedException}
import java.io.FileOutputStream

object TestFileMaker extends IOApp {
    /** random string between mn and mx */
    def fileName(mn: Int = 3, mx: Int = 8): IO[String] = 
        IO(alphanumeric.take(nextInt(mx-mn) + mn).mkString)

    /** fname: folder name, can use multiple subdirectories together */
    def makeFolder(fname: String): IO[Unit] = {
        def job = {
            // new File(fname).mkdirs() 
            val path = Paths.get(fname)
            Files.createDirectories(path)
        }
        IO(job).handleErrorWith(_ match {
            case e: AccessDeniedException => 
                IO(println(s"Error: $e")) *>
                IO(())
            case _ =>
                IO(())
        }).void
    }

    /** 
     * base: folder name to make files
     * fname: file name
     */
    def makeFile(base: String, fname: String): IO[Unit] = {
        val r = Resource.make(IO(new FileOutputStream(new File(s"$base/$fname"))))({ os => 
                IO({ os.flush()
                os.close()})})
        r.use { os => 
            IO({ val content = fname.getBytes()
            os.write(content) })
        }
    }

    def makeRandomFileAndPrint(base: String) = 
        for {
            fn <- fileName()
            _ <- makeFile(base, fn)
            _ <- IO(println(fn))
        } yield ()

    def run(args: List[String]): IO[ExitCode] = {
        val foldName = args.headOption.getOrElse(".")
        val fileNumber = args.drop(1).headOption.flatMap(numStr => Try(numStr.toInt).toOption).getOrElse(50)
        val p = Range(0, fileNumber).map(_ => makeRandomFileAndPrint(foldName)).toList
        (for {
            _ <- IO(println(s"at base folder : $foldName, making $fileNumber files"))
            _ <- makeFolder(foldName)
            _ <- p.sequence
            // _ <- makeFolder("testFolder/abc")
        } yield ()).as(ExitCode.Success)
    }
}