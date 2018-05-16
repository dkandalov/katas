package katas.kotlin.mukanren

data class Variable(val name: String)

data class State(
    val variables: List<Variable> = emptyList(),
    val values: Map<Variable, Any> = emptyMap()
) {
    fun createVariables(names: List<String>): State {
        val newVariables = names.map { Variable(it) }
        return copy(variables = variables + newVariables)
    }

    fun assignValues(vararg newValues: Pair<Variable, Any>): State {
        return copy(values = values + newValues)
    }

    fun valueOf(key: Any): Any {
        return if (key is Variable && values.containsKey(key)) values[key]!! else key
    }

    fun unify(a: Any, b: Any): State {
        val aValue = valueOf(a)
        val bValue = valueOf(b)
        return if (aValue == bValue) this
        else if (aValue is Variable) assignValues(Pair(aValue, bValue))
        else if (bValue is Variable) assignValues(Pair(bValue, aValue))
        else error("")
    }
}
