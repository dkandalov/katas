@file:Suppress("PackageDirectoryMismatch")
package tapl.chapter5

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.util.*


interface Term
interface Value

data class Var(val name: String): Term {
    override fun toString() = name
}
data class Lambda(val arg: Var, val body: Term): Term, Value {
    override fun toString() = "λ$arg.$body"
}
data class Apply(val t1: Term, val t2: Term): Term {
    override fun toString() = "($t1)($t2)"
}

fun Term.eval(): Term = when {
    this is Apply && t1.isReducible() -> Apply(t1.eval(), t2) // E-App1
    this is Apply && t1 is Value && t2.isReducible() -> Apply(t1, t2.eval()) // E-App2
    this is Apply && t1 is Lambda && t2 is Value -> t1.body.substitute(t1.arg, t2) // E-AppAbs
    else -> this
}

fun Term.isReducible() = this != eval()

fun Term.substitute(arg: Var, term: Term): Term {
    if (this is Var && this.name == arg.name) return term
    if (this is Lambda) {
        val tempVar = Var(Random().nextLong().toString())
        val lambda = this.rename(this.arg, tempVar) as Lambda
        return lambda.copy(body = lambda.body.substitute(arg, term))
                .rename(this.arg, Var(this.arg.name + this.arg.name))
                .rename(tempVar, this.arg)
    }
    if (this is Apply) return Apply(t1.substitute(arg, term), t2.substitute(arg, term))
    return this
}

fun Term.rename(from: Var, to: Var): Term {
    if (this is Var && this.name == from.name) return to
    if (this is Lambda) return copy(body = body.rename(from, to))
    if (this is Apply) return Apply(t1.rename(from, to), t2.rename(from, to))
    return this
}

fun λ(argName: String, varName: String) = λ(argName, Var(varName))
fun λ(argName: String, t: Term) = Lambda(Var(argName), t)
fun 𝑣(name: String) = Var(name)
operator fun Term.invoke(varName: String) = this.invoke(Var(varName))
operator fun Term.invoke(t: Term) = Apply(this, t)


class EvaluationTest {
    @Test fun `variables and lambdas evaluate to themselves`() {
        𝑣("a") aka "a" evaluatesTo "a"
        λ("a", "a") aka "λa.a" evaluatesTo "λa.a"
    }

    @Test fun `variables are not applied because they are not values`() =
        λ("a", "a")("b") aka "(λa.a)(b)" evaluatesTo "(λa.a)(b)"

    @Test fun `basic application`() =
        λ("a", "a")(λ("b", "b")) aka "(λa.a)(λb.b)" evaluatesTo "λb.b"

    @Test fun `evaluation with substitute`() {
        λ("y", "x").substitute(𝑣("x"), λ("z", 𝑣("z")(𝑣("w")))) isEqualTo λ("y", λ("z", 𝑣("z")(𝑣("w"))))
        λ("x", "x").substitute(𝑣("x"), 𝑣("y")) isEqualTo λ("x", "x") // replacing bound variable
        λ("z", "x").substitute(𝑣("x"), 𝑣("z")) isEqualTo λ("z", "zz") // variable capture
        λ("y", 𝑣("x")(𝑣("y"))).substitute(𝑣("x"), 𝑣("y")(𝑣("z"))) isEqualTo λ("y", 𝑣("yy")(𝑣("z"))(𝑣("y")))
    }

    private infix fun Term.aka(termAsString: String): Term = apply {
        assertThat("input term", this.toString(), equalTo(termAsString))
    }

    private infix fun Term.evaluatesTo(termAsString: String) =
        assertThat("evaluated term", eval().toString(), equalTo(termAsString))

    private infix fun Term.evaluatesTo(expectedTerm: Term) = assertThat(eval(), equalTo(expectedTerm))

    private infix fun Term.isEqualTo(expectedTerm: Term) = assertThat(this, equalTo(expectedTerm))
}