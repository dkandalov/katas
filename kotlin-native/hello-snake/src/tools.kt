import kotlinx.cinterop.CPointer
import kotlinx.cinterop.cstr
import platform.osx.ERR
import platform.posix.FILE
import platform.posix.fopen
import platform.posix.fwrite

val fd: CPointer<FILE> = fopen("snake.log", "w+")!!

fun Int.logOnError(message: String = ""): Int {
    if (this == ERR) {
        val logMessage = ERR.toString() + ": " + message + "\n"
        fwrite(logMessage.cstr, 1, logMessage.length.toLong(), fd)
    }
    return this
}
