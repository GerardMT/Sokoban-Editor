package gmt.terminal

import java.io.File

object Main {

    def main(args: Array[String]): Unit = {
        val settingsPath = new File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getPath + "/config"

    }
}
