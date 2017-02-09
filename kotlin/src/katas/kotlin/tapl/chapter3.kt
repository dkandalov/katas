/*
This file contains implementation of language described in Chapter 3
of the Types and Programming Languages book (https://www.cis.upenn.edu/~bcpierce/tapl/)
*/

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

interface Term

object `true`: Term
object `false`: Term
data class `if`(val predicate: Term, val then: Term, val `else`: Term): Term

object zero : Term
data class succ(val t: Term): Term
data class pred(val t: Term): Term
data class iszero(val t: Term): Term


fun Term.eval(): Term =
    when {
        this is `if` -> when {
            predicate == `true` -> then                                      // E-IfTrue
            predicate == `false` -> `else`                                   // E-IfFalse
            predicate.canEval() -> copy(predicate = predicate.eval()).eval() // E-If
            else -> this
        }
        this is succ && t.canEval() -> copy(t.eval()).eval()                 // E-Succ

        this == pred(zero) -> zero                                           // E-PredZero
        this is pred && t is succ && t.t.isNumericValue() -> t.t             // E-PredSucc
        this is pred && t.canEval() -> copy(t.eval()).eval()                 // E-Pred

        this == iszero(zero) -> `true`                                       // E-IszeroZero
        this is iszero && t is succ && t.t.isNumericValue() -> `false`       // E-IszeroSucc
        this is iszero && t.canEval() -> copy(t.eval()).eval()               // E-IsZero

        else -> this
    }

fun Term.canEval() = !isValue() && this != this.eval()

fun Term.isValue() =
        this == `true` || this == `false` || isNumericValue()

fun Term.isNumericValue(): Boolean =
        this == zero || (this is succ && t.isNumericValue())


class EvaluationTest {
    private val t2 = namedValue("t2")
    private val t3 = namedValue("t3")
    private val normalFormTerm = `if`(zero, `true`, `false`)

    init {
        normalFormTerm.assertIsNormalForm()
    }

    @Test fun `values evaluate to themselves`() {
        `true` evaluatesTo `true`
        `false` evaluatesTo `false`
        zero evaluatesTo zero
    }

    @Test fun `E-IfTrue, E-IfFalse`() {
        `if`(`true`, then = t2, `else` = t3) evaluatesTo t2
        `if`(`false`, then = t2, `else` = t3) evaluatesTo t3
    }

    @Test fun `E-If`() {
        `if`(termWhichEvalsTo(`true`), then = t2, `else` = t3) evaluatesTo t2
        `if`(normalFormTerm, then = t2, `else` = t3).assertIsNormalForm()
    }

    @Test fun `E-Succ`() {
        succ(termWhichEvalsTo(zero)) evaluatesTo succ(zero)
        succ(normalFormTerm).assertIsNormalForm()
    }

    @Test fun `E-PredZero`() {
        pred(zero) evaluatesTo zero
        pred(`true`).assertIsNormalForm()
    }

    @Test fun `E-PredSucc`() {
        pred(succ(zero)) evaluatesTo zero
        pred(succ(succ(zero))) evaluatesTo succ(zero)
        pred(succ(`true`)).assertIsNormalForm() // because "true" is not "nv" (numeric value)
    }

    @Test fun `E-Pred`() {
        pred(termWhichEvalsTo(succ(zero))) evaluatesTo zero
        pred(normalFormTerm).assertIsNormalForm()
    }

    @Test fun `E-IszeroZero`() {
        iszero(zero) evaluatesTo `true`
    }

    @Test fun `E-IszeroSucc`() {
        iszero(succ(zero)) evaluatesTo `false`
        iszero(succ(`true`)).assertIsNormalForm()
    }

    @Test fun `E-IsZero`() {
        iszero(termWhichEvalsTo(zero)) evaluatesTo `true`
        iszero(termWhichEvalsTo(succ(zero))) evaluatesTo `false`
        iszero(normalFormTerm).assertIsNormalForm()
    }

    private fun termWhichEvalsTo(term: Term) = `if`(`true`, term, `false`)

    private infix fun Term.evaluatesTo(expectedTerm: Term) = assertThat(eval(), equalTo(expectedTerm))

    private fun Term.assertIsNormalForm() = assertThat(this.eval(), equalTo(this))

    private fun namedValue(name: String) = object : Term {
        override fun toString() = name
    }
}
