@file:Suppress("PackageDirectoryMismatch")
package katas.kotlin.tapl.chapter11

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.containsSubstring
import com.natpryce.hamkrest.equalTo
import katas.kotlin.shouldEqual
import katas.kotlin.tapl.chapter11.TestUtil.aka
import katas.kotlin.tapl.chapter11.TestUtil.evaluatesTo
import katas.kotlin.tapl.chapter11.TestUtil.invoke
import katas.kotlin.tapl.chapter11.TestUtil.v
import katas.kotlin.tapl.chapter11.TestUtil.λ
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
object unit: Term, Value {
    override fun toString() = "unit"
}
data class Ascribe(val t: Term, val type: TermType): Term {
    override fun toString() = "$t as $type"
}
data class Let(val x: Var, val t1: Term, val t2: Term): Term {
    override fun toString() = "let $x=$t1 in $t2"
}
data class APair(val t1: Term, val t2: Term): Term {
    override fun toString() = "{$t1,$t2}"
}


interface DerivedForm {
    fun derive(): Term
}
data class Seq(val t1: Term, val t2: Term): Term, DerivedForm {
    override fun derive() = Apply(Lambda(Var("x"), UnitType, t2), t1)
    override fun toString() = "$t1; $t2"
}
object Wildcard: Term, DerivedForm {
    override fun derive() = Var("unused")
    override fun toString() = "_"
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
data class BaseType(val name: String): TermType {
    override fun toString() = "BaseType($name)"
}
object UnitType: TermType {
    override fun toString() = "Unit"
}
data class PairType(val type1: TermType, val type2: TermType): TermType {
    override fun toString() = "${type1}X$type2"
}


fun Term.type(Γ: Map<Var, TermType> = emptyMap()): TermType = when {
    // Rules from figure 8.2
    this is `true` -> Bool                                  // T-True
    this is `false` -> Bool                                 // T-False
    this is `if` && predicate.type(Γ) is Bool &&
            then.type(Γ) == `else`.type(Γ) -> then.type(Γ)  // T-If
    this is zero -> Nat                                     // ???
    this is unit -> UnitType                                // T-Unit
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
            error("Cannot infer type of: '$this' where Γ is $Γ. " +
                  "Argument type '${type12.from}' is not equal body type '$type11'.")
        } else {
            type12.to
        }
    }
    this is Ascribe -> t.type(Γ).also {                     // T-Ascribe
        if (it != type) error("Ascribed type '$it' is not equal to inferred type '$type'")
    }
    this is Let -> t2.type(Γ + (x to t1.type(Γ)))           // T-Let
    this is APair -> PairType(t1.type(Γ), t2.type(Γ))

    this is DerivedForm -> derive().type(Γ)

    else -> error("Cannot infer type of: '$this' where Γ is $Γ")
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

    this is Ascribe -> t                                                 // E-Ascribe
    this is Let -> t2.substitute(x, t1.eval())                           // E-Let
    this is APair -> APair(t1.eval(), t2.eval())

    this is DerivedForm -> derive().eval()

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
    this is Lambda -> copy(body = body.substitute(arg, term))
    this is `if` -> copy(predicate.substitute(arg, term), then.substitute(arg, term), `else`.substitute(arg, term))
    this is isZero -> copy(t.substitute(arg, term))
    this is succ -> copy(t.substitute(arg, term))
    this is pred -> copy(t.substitute(arg, term))
    this is Ascribe -> copy(t.substitute(arg, term))
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
        unit hasType UnitType

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
        succ(v("b")).hasNoValidType()

        λ("x:Bool", `true`) hasType "Bool->Bool"
        λ("x:Bool", zero) hasType "Bool->Nat"
        λ("x:Bool", λ("y:Nat", `true`)) hasType "Bool->Nat->Bool"
        λ("x", FunctionType(Nat, Nat), λ("y:Bool", "x")) aka "λx:Nat->Nat.λy:Bool.x" hasType "Nat->Nat->Bool->Nat->Nat"

        λ("x:Bool", `true`)(`true`) hasType Bool
        λ("x:Bool", `true`)(zero).hasNoValidType()
        λ("a:Bool", λ("a:Nat", "a"))("b") aka "(λa:Bool.λa:Nat.a)(b)" hasType "Nat->Nat"
        λ("a:Bool", λ("a:Nat", "b"))("b") aka "(λa:Bool.λa:Nat.b)(b)" hasType "Nat->Bool"
        Lambda(Wildcard.derive(), Bool, zero)(`true`) hasType Nat

        Seq(`true`, zero).hasNoValidType()
        Seq(unit, `false`) hasType Bool

        Ascribe(succ(zero), Nat) hasType Nat
        Ascribe(succ(zero), Bool).hasNoValidType("Ascribed type 'Nat' is not equal to inferred type 'Bool'")

        Let(v("x"), `true`, v("x")) hasType Bool
        Let(v("x"), `true`, zero) hasType Nat

        APair(`true`, zero) hasType PairType(Bool, Nat)
    }

    private val Γ = mapOf(
        Var("b") to Bool,
        Var("n") to Nat
    )

    private infix fun Term.hasType(expectedType: TermType) = type(Γ) shouldEqual expectedType

    private infix fun Term.hasType(expectedType: String) = type(Γ).toString() shouldEqual expectedType

    private fun Term.hasNoValidType(expectedError: String = "Cannot infer type") {
        try {
            val termType = type(Γ)
            fail("Expected type error but was '$termType'")
        } catch (e: IllegalStateException) {
            assertThat(e.message!!, containsSubstring(expectedError))
        }
    }
}

class TermEvaluationTests {
    @Test fun `values evaluate to themselves`() {
        `true` evaluatesTo "true"
        `false` evaluatesTo "false"
        zero evaluatesTo "0"
        unit evaluatesTo "unit"
    }

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

    @Test fun `ascription doesn't affect evaluation`() {
        Ascribe(pred(succ(zero)), Nat) evaluatesTo "0"
    }

    @Test fun `evaluation of 'let'`() {
        Let(Var("two"), succ(succ(zero)), isZero(Var("two"))) aka "let two=succ(succ(0)) in isZero(two)" evaluatesTo "false"
        Let(Var("t"), `true`, `if`(Var("t"), succ(zero), zero)) aka "let t=true in if (t) succ(0) else 0" evaluatesTo "succ(0)"
    }

    @Test fun `evaluation of pair`() {
        APair(pred(succ(zero)), succ(pred(succ(zero)))) aka "{pred(succ(0)),succ(pred(succ(0)))}" evaluatesTo "{0,succ(0)}"
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