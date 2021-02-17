package io.nineclue.filemover

import java.nio.file.Path

trait FileAttributes[A] {
    type FileAttribute = A
    def attributes(f: Path): List[FileAttribute]
    def folderName(as: List[FileAttribute]): String
    def apply(f: Path) = folderName(attributes(f))
}