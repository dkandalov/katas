@file:Suppress("PackageDirectoryMismatch")
package tapl.chapter5

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.util.Random


interface Term
interface Value

data class Var(val name: String): Term
data class Lambda(val arg: Var, val t: Term): Term, Value
data class Apply(val t1: Term, val t2: Term): Term

fun Term.eval(): Term = when {
    this is Apply && t1.reducible() -> Apply(t1.eval(), t2) // E-App1
    this is Apply && t1 is Value && t2.reducible() -> Apply(t1, t2.eval()) // E-App2
    this is Apply && t1 is Lambda && t2 is Value -> t1.t.substitute(t1.arg, t2) // E-AppAbs
    else -> this
}

fun Term.reducible() = this != eval()

fun Term.substitute(arg: Var, value: Term): Term {
    if (this is Var && this.name == arg.name) return value
    if (this is Lambda) {
        val tempVar = Var(Random().nextLong().toString())
        val l = rename(this.arg, tempVar) as Lambda
        return l.copy(t = l.t.substitute(arg, value))
                .rename(this.arg, Var(this.arg.name + this.arg.name))
                .rename(tempVar, this.arg)
    }
    if (this is Apply) return Apply(t1.substitute(arg, value), t2.substitute(arg, value))
    return this
}

fun Term.rename(from: Var, to: Var): Term {
    if (this is Var && this.name == from.name) return to
    if (this is Lambda) return copy(t = t.rename(from, to))
    if (this is Apply) return Apply(t1.rename(from, to), t2.rename(from, to))
    return this
}

fun λ(argName: String, t: Term) = Lambda(Var(argName), t)
fun 𝑣(name: String) = Var(name)
operator fun Term.invoke(t: Term) = Apply(this, t)


class EvaluationTest {
    @Test fun `evaluation with substitute`() {
        𝑣("a") evaluatesTo 𝑣("a")
        λ("a", 𝑣("a")) evaluatesTo λ("a", 𝑣("a"))

        λ("a", 𝑣("a"))(λ("b", 𝑣("b"))) evaluatesTo λ("b", 𝑣("b"))

        λ("y", 𝑣("x")).substitute(𝑣("x"), λ("z", 𝑣("z")(𝑣("w")))) isEqualTo λ("y", λ("z", 𝑣("z")(𝑣("w"))))
        λ("x", 𝑣("x")).substitute(𝑣("x"), 𝑣("y")) isEqualTo λ("x", 𝑣("x")) // replacing bound variable
        λ("z", 𝑣("x")).substitute(𝑣("x"), 𝑣("z")) isEqualTo λ("z", 𝑣("zz")) // variable capture
        λ("y", 𝑣("x")(𝑣("y"))).substitute(𝑣("x"), 𝑣("y")(𝑣("z"))) isEqualTo λ("y", 𝑣("yy")(𝑣("z"))(𝑣("y")))
    }

    private infix fun Term.evaluatesTo(expectedTerm: Term) = assertThat(eval(), equalTo(expectedTerm))
    
    private infix fun Term.isEqualTo(expectedTerm: Term) = assertThat(this, equalTo(expectedTerm))
}