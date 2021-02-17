package io.nineclue.filemover

import java.nio.file.Path

object FileNameAttributes extends FileAttributes[String] {
    def attributes(p: Path): List[FileAttribute] = List(p.getFileName().toString)
    def folderName(as: List[FileAttribute]): String = as.head.head.toString.toUpperCase()
}