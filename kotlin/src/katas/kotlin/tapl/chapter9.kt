@file:Suppress("PackageDirectoryMismatch")
package katas.kotlin.tapl.chapter9

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.tapl.chapter9.TestUtil.aka
import katas.kotlin.tapl.chapter9.TestUtil.evaluatesTo
import katas.kotlin.tapl.chapter9.TestUtil.invoke
import katas.kotlin.tapl.chapter9.TestUtil.v
import katas.kotlin.tapl.chapter9.TestUtil.λ
import datsok.shouldEqual
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

interface Term
interface Value
interface NumericValue : Value


data class Var(val name: String): Term, Value {
    override fun toString() = name
}
data class Lambda(val arg: Var, val argType: TermType, val body: Term): Term, Value {
    override fun toString() = "λ$arg:$argType.$body"
}
data class Apply(val t1: Term, val t2: Term): Term {
    override fun toString() = "($t1)($t2)"
}
object `true`: Term {
    override fun toString() = "true"
}
object `false`: Term {
    override fun toString() = "false"
}
data class `if`(val predicate: Term, val then: Term, val `else`: Term): Term {
    override fun toString() = "if ($predicate) $then else $`else`"
}
object zero: Term, Value, NumericValue {
    override fun toString() = "0"
}
data class succ(val t: Term): Term, NumericValue {
    override fun toString() = "succ($t)"
}
data class pred(val t: Term): Term, Value {
    override fun toString() = "pred($t)"
}
data class isZero(val t: Term): Term, Value {
    override fun toString() = "isZero($t)"
}


interface TermType
object Bool: TermType {
    override fun toString() = "Bool"
}
object Nat: TermType {
    override fun toString() = "Nat"
}
data class FunctionType(val from: TermType, val to: TermType) : TermType {
    override fun toString() = "$from->$to"
}


fun Term.type(Γ: Map<Var, TermType> = emptyMap()): TermType = when {
    // Rules from figure 8.2
    this is `true` -> Bool                                  // T-True
    this is `false` -> Bool                                 // T-False
    this is `if` && predicate.type(Γ) is Bool &&
            then.type(Γ) == `else`.type(Γ) -> then.type(Γ)  // T-If
    this is zero -> Nat                                     // ???
    this is succ && t.type(Γ) is Nat -> Nat                 // T-Succ
    this is pred && t.type(Γ) is Nat -> Nat                 // T-Pred
    this is isZero && t.type(Γ) is Nat -> Bool              // T-IsZero

    // Rules from figure 9.1
    this is Var && Γ.contains(this) -> Γ[this]!!            // T-Var
    this is Lambda -> {                                     // T-Abs
        val type1 = argType
        val type2 = body.type(Γ + (arg to argType))
        FunctionType(type1, type2)
    }
    this is Apply -> {                                      // T-App
        val type12 = t1.type(Γ) as FunctionType
        val type11 = t2.type(Γ)
        if (type12.from != type11) {
            error("Can't infer type of: '$this' where Γ is $Γ")
        } else {
            type12.to
        }
    }

    else -> error("Can't infer type of: '$this' where Γ is $Γ")
}

fun Term.eval(): Term = when {
    // Rules from Figure 3.1 and 3.2
    this is `if` -> when {
        predicate == `true` -> then                                      // E-IfTrue
        predicate == `false` -> `else`                                   // E-IfFalse
        predicate.isReducible() -> copy(predicate = predicate.eval())    // E-If
        else -> this
    }
    this is succ && t.isReducible() -> copy(t.eval())                    // E-Succ
    this == pred(zero) -> zero                                           // E-PredZero
    this is pred && t is succ && t.t.isNumericValue() -> t.t             // E-PredSucc
    this is pred && t.isReducible() -> copy(t.eval())                    // E-Pred
    this == isZero(zero) -> `true`                                       // E-IszeroZero
    this is isZero && t is succ && t.t.isNumericValue() -> `false`       // E-IszeroSucc
    this is isZero && t.isReducible() -> copy(t.eval())                  // E-IsZero

    // Rules from figure 9.1
    this is Apply && t1.isReducible() -> Apply(t1.eval(), t2)                      // E-App1
    this is Apply && t1 is Value && t2.isReducible() -> Apply(t1, t2.eval())       // E-App2
    this is Apply && t1 is Lambda && t2 is Value -> t1.body.substitute(t1.arg, t2) // E-AppAbs

    else -> this
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

fun Term.substitute(arg: Var, term: Term): Term = when {
    this is Var && this.name == arg.name -> term
    this is Apply -> Apply(t1.substitute(arg, term), t2.substitute(arg, term))
    this is Lambda -> this.copy(body = this.body.substitute(arg, term))
    else -> this
}

fun Term.isReducible() = !isValue() && this != this.eval()
fun Term.isValue() = this == `true` || this == `false` || isNumericValue()
fun Term.isNumericValue(): Boolean = this == zero || (this is succ && t.isNumericValue())


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
    this is Lambda -> Lambda(arg.intsToVarNames(context) as Var, argType, body.intsToVarNames(context))
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
        Pair(Lambda(Var(context1.max.toString()), argType, body), context2.dropStackFrame())
    }
    else -> Pair(this, context)
}


class TermTypeTests {
    @Test fun `type of a term`() {
        `false` hasType Bool
        `true` hasType Bool
        zero hasType Nat
        v("b") hasType Bool
        v("n") hasType Nat

        `if`(`true`, then = `true`, `else` = `false`) hasType Bool
        `if`(`true`, then = `true`, `else` = `false`) hasType Bool
        `if`(`true`, then = zero, `else` = zero) hasType Nat
        `if`(`true`, then = Var("b"), `else` = `false`) hasType Bool
        `if`(`true`, then = zero, `else` = `false`).hasNoValidType()
        `if`(`true`, then = Var("b"), `else` = Var("n")).hasNoValidType()

        isZero(succ(zero)) hasType Bool
        isZero(`false`).hasNoValidType()
        isZero(v("a")).hasNoValidType()

        succ(zero) hasType Nat
        pred(zero) hasType Nat
        pred(succ(succ(zero))) hasType Nat
        succ(Var("b")).hasNoValidType()

        λ("x:Bool", `true`) hasType "Bool->Bool"
        λ("x:Bool", zero) hasType "Bool->Nat"
        λ("x:Bool", λ("y:Nat", `true`)) hasType "Bool->Nat->Bool"
        λ("x", FunctionType(Nat, Nat), λ("y:Bool", "x")) aka "λx:Nat->Nat.λy:Bool.x" hasType "Nat->Nat->Bool->Nat->Nat"

        λ("x:Bool", `true`)(`true`) hasType Bool
        λ("x:Bool", `true`)(zero).hasNoValidType()
        λ("a:Bool", λ("a:Nat", "a"))("b") aka "(λa:Bool.λa:Nat.a)(b)" hasType "Nat->Nat"
        λ("a:Bool", λ("a:Nat", "b"))("b") aka "(λa:Bool.λa:Nat.b)(b)" hasType "Nat->Bool"
    }

    private val Γ = mapOf(
        Var("b") to Bool,
        Var("n") to Nat
    )

    private infix fun Term.hasType(expectedType: TermType) = type(Γ) shouldEqual expectedType

    private infix fun Term.hasType(expectedType: String) = type(Γ).toString() shouldEqual expectedType

    private fun Term.hasNoValidType() {
        try {
            type(Γ)
            fail("Expected IllegalStateException")
        } catch (e: IllegalStateException) {
            assertTrue(e.message!!.startsWith("Can't infer type"))
        }
    }
}

class TermEvaluationTests {
    @Test fun `variables and lambdas evaluate to themselves`() {
        v("a") aka "a" evaluatesTo "a"
        λ("a:Bool", "a") aka "λa:Bool.a" evaluatesTo "λa:Bool.a"
    }

    @Test fun `basic application`() {
        λ("a:Nat", "a")("x") aka "(λa:Nat.a)(x)" evaluatesTo "x"
        λ("a:Nat", "a")(λ("x:Bool", "x")) aka "(λa:Nat.a)(λx:Bool.x)" evaluatesTo "λx:Bool.x"
    }

    @Test fun `application of free and bound variables`() {
        //                                       unused argument
        //                                       ↓
        λ("a:Bool", λ("a:Nat", "a"))("x") aka "(λa:Bool.λa:Nat.a)(x)" evaluatesTo "λa:Nat.a"
        λ("a:Bool", λ("b:Nat", "a"))("x") aka "(λa:Bool.λb:Nat.a)(x)" evaluatesTo "λb:Nat.x"
    }

    @Test fun `application avoids variable capture`() {
        //                                                     have the same name but different meaning
        //                                                     ↓        ↓
        λ("a:Bool", λ("b:Nat", "a"))("b")("c") aka "((λa:Bool.λb:Nat.a)(b))(c)" evaluatesTo "b"
    }
}

object TestUtil {
    fun v(name: String) = Var(name)

    fun λ(argName: String, argType: TermType, t: Term) = Lambda(Var(argName), argType, t)

    fun λ(arg: String, varName: String) = λ(arg, Var(varName))

    fun λ(arg: String, t: Term): Lambda {
        val (argName, type) = arg.split(":").let {
            if (it.size != 2) error("Expected arg with name and type but it was '$arg'") else it
        }
        val argType = when (type) {
            "Nat" -> Nat
            "Bool" -> Bool
            else -> error("Unknown type $type")
        }
        return Lambda(Var(argName), argType, t)
    }

    operator fun Term.invoke(varName: String) = this.invoke(Var(varName))

    operator fun Term.invoke(t: Term) = Apply(this, t)

    infix fun Term.aka(termAsString: String): Term = apply {
        assertThat("input term", this.toString(), equalTo(termAsString))
    }

    infix fun Term.evaluatesTo(termAsString: String) =
        assertThat("evaluated term", fullEval().toString(), equalTo(termAsString))
}