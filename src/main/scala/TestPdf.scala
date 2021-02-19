package io.nineclue.filemover

import cats.effect._
import cats.implicits._
import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentCatalog, PDDocumentInformation}
import org.apache.pdfbox.pdmodel.common.PDMetadata
import org.apache.xmpbox.xml.DomXmpParser
import java.io.File
import java.nio.file.Paths
import FileUtilIO._
import org.apache.xmpbox.XMPMetadata
import org.apache.xmpbox.schema.DublinCoreSchema
import scala.jdk.CollectionConverters._

object TestPdf extends IOApp {
    val xmpParser = new DomXmpParser()

    def openpdf(f: File) = 
        Resource.make(IO(PDDocument.load(f)))(pdf => IO(pdf.close()))

    def extractMeta(doc: PDDocument) = 
        IO(doc.getDocumentCatalog().getMetadata())

    def getODublinCoreSchema(md: XMPMetadata) = {
        val odc = Option(md.getDublinCoreSchema())
        odc.map({ dc => 
            DublinCore(dc.getTitle(), dc.getDescription(),
                dc.getCreators().asScala.toList, dc.getDates().asScala.toList, 
                dc.getSubjects().asScala.toList)
        })
    }

    def getPDocInfo(doc: PDDocument) = {
        val pi = doc.getDocumentInformation()
        PDocInfo(pi.getTitle(), pi.getSubject(), pi.getAuthor(), 
            pi.getCreator(), pi.getProducer())
    }

    def info(f: File) = {
        openpdf(f).use( { doc =>
            for {
                meta <- extractMeta(doc)
                metadata = xmpParser.parse(meta.toByteArray())
                // odc = getODublinCoreSchema(metadata)
                pi = getPDocInfo(doc)
            } yield (f.getName, pi)
        })
    }

    def run(args: List[String]): IO[ExitCode] = {
        {
            for {
                files <- listFiles(Paths.get("."))
                finfo <- files.traverse(p => info(p.toFile()).attempt)
                _ <- IO(println(finfo))
            } yield ()
        }.handleErrorWith({ case e: Throwable =>
            IO.unit
        }).as(ExitCode.Success)
    }
}