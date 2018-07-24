import kotlinx.cinterop.CPointer
import kotlinx.cinterop.cstr
import kotlinx.cinterop.staticCFunction
import platform.osx.ERR
import platform.posix.*

val fd: CPointer<FILE> = fopen("snake.log", "a+")!!.also {
    atexit(staticCFunction(::handleExit))
}
private fun handleExit() {
    fclose(fd)
    println("bye!")
}


fun <T> T.logOnError(message: String = ""): T {
    if (this != ERR) return this
    fwrite(message.cstr, 1, message.length.toLong(), fd)
    IllegalStateException().printStackTrace()
    TODO()
}
