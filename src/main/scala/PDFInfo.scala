package io.nineclue.filemover

import java.util.Calendar

sealed trait PDFInfo

case class DublinCore(title: String, description: String, 
    creators: List[String], dates: List[Calendar], subjects: List[String]
    ) extends PDFInfo

case class PDocInfo(title: String, subject: String, author: String, 
    creator: String, producer: String) extends PDFInfo
