@file:Suppress("PackageDirectoryMismatch")
package tapl.chapter5

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.shouldEqual
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
    var (lastResult, context) = this.varNamesToInts()
    var result = lastResult.eval()
    while (result != lastResult) {
        lastResult = result
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
) {
    fun addMapping(name: String): Context {
        val i = max + 1
        val pair = Pair(i, name)
        return copy(mapping = mapping + pair, max = i)
    }

    fun addStackFrame(name: String) = copy(stack = stack + Pair(max, name))

    fun dropStackFrame() = copy(stack = stack.dropLast(1))

    fun lookupOnStack(name: String): Int? = stack.findLast{ it.second == name }?.first
}

private fun Term.intsToVarNames(context: Context = Context()): Term = when {
    this is Var -> Var(context.mapping[name.toInt()]!!)
    this is Apply -> Apply(t1.intsToVarNames(context), t2.intsToVarNames(context))
    this is Lambda -> Lambda(arg.intsToVarNames(context) as Var, body.intsToVarNames(context))
    else -> this
}

private fun Term.varNamesToInts(context: Context = Context()): Pair<Term, Context> = when {
    this is Var -> {
        val i = context.lookupOnStack(name)
        if (i != null) {
            Pair(Var(i.toString()), context)
        } else {
            val updatedContext = context.addMapping(name)
            Pair(Var(updatedContext.max.toString()), updatedContext)
        }
    }
    this is Apply -> {
        val (t1, context2) = this.t1.varNamesToInts(context)
        val (t2, context3) = this.t2.varNamesToInts(context2)
        Pair(Apply(t1, t2), context3)
    }
    this is Lambda -> {
        val context1 = context.addMapping(arg.name).addStackFrame(arg.name)
        val (body, context2) = body.varNamesToInts(context1)
        Pair(Lambda(Var(context1.max.toString()), body), context2.dropStackFrame())
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
        v("a").varNamesToInts().first.toString() shouldEqual "0"

        λ("a", "a").aka("λa.a").varNamesToInts().let { (term, context) ->
            term.toString() shouldEqual "λ0.0"
            context.mapping shouldEqual mapOf(0 to "a")
        }

        λ("a", "a")("a").aka("(λa.a)(a)").varNamesToInts().let { (term, context) ->
            term.toString() shouldEqual "(λ0.0)(1)"
            context.mapping shouldEqual mapOf(0 to "a", 1 to "a")
        }

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

    @Test fun `substitution`() {
        λ("y", "x").substitute(v("x"), λ("z", v("z")(v("w")))) isEqualTo λ("y", λ("z", v("z")(v("w"))))
    }

    private infix fun Term.aka(termAsString: String): Term = apply {
        assertThat("input term", this.toString(), equalTo(termAsString))
    }

    private infix fun Term.evaluatesTo(termAsString: String) =
        assertThat("evaluated term", fullEval().toString(), equalTo(termAsString))

    private infix fun Term.isEqualTo(expectedTerm: Term) = assertThat(this, equalTo(expectedTerm))
}