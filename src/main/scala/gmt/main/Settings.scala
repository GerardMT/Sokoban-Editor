package gmt.main

object Settings {

    case class NoSeparatorException(line: Int) extends Exception
    case class MultipleSeparatorException(line: Int) extends Exception
    case class KeyNotFoundException(key: String) extends Exception

    private val SEPARATOR = '='

    private val KEY_YICES_2_PATH = "yices2_path"

    def from(string: String): Settings = {
        val lines = string.split("\n").filter(f => f.head != ';').toList

        lines.zipWithIndex.foreach(f => {
            if (!f._1.contains(SEPARATOR)) {
                throw NoSeparatorException(f._2)
            }
        })

        val settingsSplit = lines.map(f => f.split(SEPARATOR))

        settingsSplit.zipWithIndex.foreach(f => {
            if (f._1.length > 2) {
                throw MultipleSeparatorException(f._2)
            }
        })

        val settingsMap = settingsSplit.map(f => {
            if (f.length == 2) {
                (f(0), f(1))
            } else {
                (f(0), "")
            }
        }).toMap

        Settings(getValue(settingsMap, KEY_YICES_2_PATH))
    }

    private def getValue(map: Map[String, String], key: String): Option[String] = {
        map.get(KEY_YICES_2_PATH) match {
            case Some("") =>
                None
            case Some(s) =>
                Some(s)
            case None =>
                throw KeyNotFoundException(KEY_YICES_2_PATH)
        }
    }
}

case class Settings private (yices2Path: Option[String])


