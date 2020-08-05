// https://gist.github.com/bertschneider/e6fadb06d80fb673e84217bf99f2c53d

fun main() { val q = """
fun main() { val q = '''%s'''
println(q.replace(Regex("'{3}"), "\"\"\"").format(q)) }"""
println(q.replace(Regex("'{3}"), "\"\"\"").format(q)) }