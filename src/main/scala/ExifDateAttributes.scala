package io.nineclue.filemover

import java.nio.file.Path
import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifSubIFDDirectory
import java.util.{Date, Calendar}

object ExifDateAttributes extends FileAttributes[Option[Date]] {
    private val defCal = Calendar.getInstance()

    def attributes(p: Path): List[FileAttribute] = {
        val metas = ImageMetadataReader.readMetadata(p.toFile())
        val d = Option(metas.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory])) // handles null
        List(d.flatMap(dir => Option(dir.getDateOriginal())))
    }

    def folderName(as: List[FileAttribute]): String = {
        for {
            ofa <- as.headOption
            fa <- ofa
            _ = defCal.setTime(fa)
            y = defCal.get(Calendar.YEAR)
            m = defCal.get(Calendar.MONTH)
        } yield String.format("%04d-%02d", y, m)
    }.getOrElse("UNKNOWN")
}