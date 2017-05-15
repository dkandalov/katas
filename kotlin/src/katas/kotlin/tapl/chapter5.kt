@file:Suppress("PackageDirectoryMismatch")
package tapl.chapter5

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.shouldEqual
import org.junit.Ignore
import org.junit.Test


interface Term
interface Value

// Note that unlike language definition in TAPL, here Var is Value
// (because it's more convenient for testing and in general it seems you should be able to apply Vars)
data class Var(val name: String): Term, Value {
    override fun toString() = name
}
data class Lambda(val arg: Var, val body: Term): Term, Value {
    override fun toString() = "λ$arg.$body"
}
data class Apply(val t1: Term, val t2: Term): Term {
    override fun toString() = "($t1)($t2)"
}

fun Term.fullEval(): Term {
    var (result, context) = this.varNamesToInts()
    // TODO refactor
    while (result != result.eval()) {
        result = result.eval()
    }
    return result.intsToVarNames(context)
}

fun Term.eval(): Term = when {
    this is Apply && t1.isReducible() -> Apply(t1.eval(), t2) // E-App1
    this is Apply && t1 is Value && t2.isReducible() -> Apply(t1, t2.eval()) // E-App2
    this is Apply && t1 is Lambda && t2 is Value -> t1.body.substitute(t1.arg, t2) // E-AppAbs
    else -> this
}

fun Term.isReducible() = this != eval()

fun Term.substitute(arg: Var, term: Term): Term = when {
    this is Var && this.name == arg.name -> term
    this is Apply -> Apply(t1.substitute(arg, term), t2.substitute(arg, term))
    this is Lambda -> this.copy(body = this.body.substitute(arg, term))
    else -> this
}

private data class Context(
    val stack: List<Pair<Int, String>> = emptyList(),
    val mapping: Map<Int, String> = emptyMap(),
    val max: Int = -1
)

private fun Term.intsToVarNames(context: Context = Context()): Term = when {
    this is Var -> Var(context.mapping[this.name.toInt()]!!)
    this is Apply -> Apply(this.t1.intsToVarNames(context), this.t2.intsToVarNames(context))
    this is Lambda -> Lambda(this.arg.intsToVarNames(context) as Var, this.body.intsToVarNames(context))
    else -> this
}

private fun Term.varNamesToInts(context: Context = Context()): Pair<Term, Context> = when {
    this is Var -> {
        val pair = context.stack.findLast{ it.second == this.name }
        if (pair != null) {
            Pair(Var(pair.first.toString()), context)
        } else {
            val i = context.max + 1
            val updatedContext = context.copy(max = i, mapping = context.mapping + Pair(i, this.name))
            Pair(Var(i.toString()), updatedContext)
        }
    }
    this is Apply -> {
        val (t1, context2) = this.t1.varNamesToInts(context)
        val (t2, context3) = this.t2.varNamesToInts(context2)
        Pair(Apply(t1, t2), context3)
    }
    this is Lambda -> {
        val i = context.max + 1
        val (body, context2) = this.body.varNamesToInts(Context(context.stack + Pair(i, this.arg.name), context.mapping + Pair(i, this.arg.name), i))
        Pair(Lambda(Var(i.toString()), body), context2.copy(stack = context2.stack.dropLast(1)))
    }
    else -> Pair(this, context)
}

fun λ(argName: String, varName: String) = λ(argName, Var(varName))
fun λ(argName: String, t: Term) = Lambda(Var(argName), t)
fun v(name: String) = Var(name)
operator fun Term.invoke(varName: String) = this.invoke(Var(varName))
operator fun Term.invoke(t: Term) = Apply(this, t)


class EvaluationTest {
    @Test fun `variables and lambdas evaluate to themselves`() {
        v("a") aka "a" evaluatesTo "a"
        λ("a", "a") aka "λa.a" evaluatesTo "λa.a"
    }

    @Test fun `basic application`() {
        λ("a", "a")("x") aka "(λa.a)(x)" evaluatesTo "x"
        λ("a", "a")(λ("x", "x")) aka "(λa.a)(λx.x)" evaluatesTo "λx.x"
    }

    @Test fun `application of free and bound variables`() {
        //                              unused argument
        //                              ↓
        λ("a", λ("a", "a"))("x") aka "(λa.λa.a)(x)" evaluatesTo "λa.a"
        λ("a", λ("b", "a"))("x") aka "(λa.λb.a)(x)" evaluatesTo "λb.x"
    }

    @Test fun `application avoids variable capture`() {
        //                                       have the same name but different meaning
        //                                       ↓    ↓
        λ("a", λ("b", "a"))("b")("c") aka "((λa.λb.a)(b))(c)" evaluatesTo "b"
    }

    @Test fun `map term variable names to unique integers`() {
        // TODO refactor

        v("a").varNamesToInts().first.toString() shouldEqual "0"
        λ("a", "a").aka("λa.a").varNamesToInts().first.toString() shouldEqual "λ0.0"
        λ("a", "a")("a").aka("(λa.a)(a)").varNamesToInts().first.toString() shouldEqual "(λ0.0)(1)"

        λ("a", "a")("b").aka("(λa.a)(b)").varNamesToInts().let { (term, context) ->
            term.toString() shouldEqual "(λ0.0)(1)"
            context.mapping shouldEqual mapOf(0 to "a", 1 to "b")
        }

        λ("a", λ("a", "a")).aka("λa.λa.a").varNamesToInts().let { (term, context) ->
            term.toString() shouldEqual "λ0.λ1.1"
            context.mapping shouldEqual mapOf(0 to "a", 1 to "a")
        }

        λ("a", λ("a", λ("a", "a"))).aka("λa.λa.λa.a").varNamesToInts().let{ (term, context) ->
            term.toString() shouldEqual "λ0.λ1.λ2.2"
            context.mapping shouldEqual mapOf(0 to "a", 1 to "a", 2 to "a")
        }
    }

    @Test fun `map integers to variable names`() {
        val context = Context(mapping = mapOf(0 to "a", 1 to "a", 2 to "a"))
        λ("0", λ("1", λ("2", "2"))).aka("λ0.λ1.λ2.2").intsToVarNames(context).toString() shouldEqual "λa.λa.λa.a"
    }

    @Ignore
    @Test fun `substitution`() {
        λ("y", "x").substitute(v("x"), λ("z", v("z")(v("w")))) isEqualTo λ("y", λ("z", v("z")(v("w"))))
        λ("x", "x").substitute(v("x"), v("y")) isEqualTo λ("x", "x") // replacing bound variable
        λ("z", "x").substitute(v("x"), v("z")) isEqualTo λ("z", "z'") // variable capture
        λ("y", v("x")(v("y"))).substitute(v("x"), v("y")(v("z"))) isEqualTo λ("y", v("y'")(v("z"))(v("y")))
    }

    private infix fun Term.aka(termAsString: String): Term = apply {
        assertThat("input term", this.toString(), equalTo(termAsString))
    }

    private infix fun Term.evaluatesTo(termAsString: String) =
        assertThat("evaluated term", fullEval().toString(), equalTo(termAsString))

    private infix fun Term.evaluatesTo(expectedTerm: Term) = assertThat(fullEval(), equalTo(expectedTerm))

    private infix fun Term.isEqualTo(expectedTerm: Term) = assertThat(this, equalTo(expectedTerm))
}