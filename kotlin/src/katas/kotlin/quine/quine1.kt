fun main() {
    val code = "fun main() {*    val code = 'x';*    println(code.replace(0x27.toChar(), 0x22.toChar()).replace(0x2A.toChar(), 0x0A.toChar()).replaceFirst('x', code))*}";
    println(code.replace(0x27.toChar(), 0x22.toChar()).replace(0x2A.toChar(), 0x0A.toChar()).replaceFirst("x", code))
}
