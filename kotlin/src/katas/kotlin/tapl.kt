interface Value
interface Term

object TRUE: Term, Value
object FALSE: Term, Value
data class IF(val predicate: Term, val then: Term, val `else`: Term): Term

object zero : Term
data class succ(val term: Term): Term
data class pred(val term: Term): Term
data class iszero(val term: Term): Term

fun Term.canEval() = this !is Value && this != this.eval()

fun Term.eval(): Term =
    when {
        this is IF -> when {
            predicate == TRUE -> then
            predicate == FALSE -> `else`
            predicate.canEval() -> copy(predicate = predicate.eval()).eval()
            else -> this
        }
        this is succ && term.canEval() -> copy(term.eval()).eval()
        this == pred(zero) -> zero
        this is pred && term is succ -> term.term
        this is pred && term.canEval() -> copy(term.eval()).eval()
        this == iszero(zero) -> TRUE
        this is iszero && term is succ -> TRUE
        this is iszero && term.canEval() -> copy(term.eval()).eval()
        else -> this
    }

fun main(args: Array<String>) {
    println(TRUE.eval())
    println(FALSE.eval())
    println(IF(TRUE, then = FALSE, `else` = TRUE).eval())
    println(IF(FALSE, then = FALSE, `else` = TRUE).eval())
    println(succ(succ(succ(zero))).eval())
    println(pred(succ(succ(succ(zero)))).eval())
    println(succ(pred(succ(succ(zero)))).eval())
}
