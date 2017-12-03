package lsystem

import org.w3c.dom.*
import org.w3c.dom.events.Event
import org.w3c.dom.events.KeyboardEvent
import kotlin.browser.document
import kotlin.browser.window
import kotlin.coroutines.experimental.buildSequence
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin


@Suppress("unused")
@JsName("main")
fun main() {
    val canvas = document.getElementById("myCanvas") as HTMLCanvasElement
    val context = canvas.getContext("2d") as CanvasRenderingContext2D
    document.body?.style?.apply {
        margin = "0"
        overflowX = "hidden"
        overflowY = "hidden"
    }
    applyTheme1(context, document)

    val presenter = LSystemPresenter()

    fun paintCanvas() {
        context.fillRect(0.0, 0.0, canvas.width.toDouble(), canvas.height.toDouble())
        context.beginPath()
        presenter.generatePoints()
            .toList().fitCenteredInto(window)
            .zipWithNext()
            .forEach { (p1, p2) ->
                if (p1 != Point.none && p2 != Point.none) {
                    context.moveTo(p1.x, p1.y)
                    context.lineTo(p2.x, p2.y)
                }
            }
        context.closePath()
        context.stroke()
    }

    paintCanvas()
    initConfigToolbar(presenter, ::paintCanvas)
    updateConfigToolbar(presenter)

    window.addEventListener("keypress", onKeyPress(presenter, context, ::paintCanvas))
    window.addEventListener("resize", { paintCanvas() }, false)
}

private fun onKeyPress(
    presenter: LSystemPresenter,
    context: CanvasRenderingContext2D,
    updateUI: () -> Unit
): (Event) -> Unit {
    val mapping = mapOf(
        "n" to { presenter.switch(1) },
        "N" to { presenter.switch(-1) },
        "d" to { presenter.changeDepth(1) },
        "D" to { presenter.changeDepth(-1) },
        "t" to { toggleConfigToolbar(document) },
        "q" to { applyTheme1(context, document) },
        "w" to { applyTheme2(context, document) }
    )
    return { event ->
        if (event is KeyboardEvent) {
            val action = mapping[event.key]
            if (action != null) {
                action()
                updateUI()
                updateConfigToolbar(presenter)
            }
        }
    }
}

fun toggleConfigToolbar(document: Document) {
    val element = document.getElementById("config-toolbar") as HTMLDivElement
    if (element.style.display == "none") {
        element.style.display = ""
    } else {
        element.style.display = "none"
    }
}

fun initConfigToolbar(presenter: LSystemPresenter, updateUI: () -> Unit) {
    inputById("apply").addEventListener("click", { _ ->
        presenter.lSystem.value.axiom = inputById("axiom").value
        presenter.lSystem.value.rules = inputById("rules").value
            .split("; ")
            .map { it.split(" => ") }
            .associate { Pair(it[0][0], it[1]) }
        presenter.lSystem.value.angle = inputById("angle").value.toDouble().toRadians()
        
        updateUI()
    })
}

fun updateConfigToolbar(presenter: LSystemPresenter) {
    inputById("axiom").value = presenter.lSystem.value.axiom
    inputById("rules").value = presenter.lSystem.value.rules
        .entries.joinToString("; ") { it.key + " => " + it.value }
    inputById("angle").value = presenter.lSystem.value.angle.toDegrees().toString()
}

fun inputById(id: String) = document.getElementById(id) as HTMLInputElement

private fun applyTheme1(context: CanvasRenderingContext2D, document: Document) {
    context.fillStyle = "#ffffff"
    context.strokeStyle = "#000000"
    document.body?.style?.background = "#ffffff"
}

private fun applyTheme2(context: CanvasRenderingContext2D, document: Document) {
    context.fillStyle = "#000000"
    context.strokeStyle = "#ffffff"
    document.body?.style?.background = "#000000"
}

class LSystemPresenter {
    val lSystems = listOf(
        ConfigurableLSystem(kochSnowflake),
        ConfigurableLSystem(quadraticType1Curve),
        ConfigurableLSystem(quadraticType2Curve),
        ConfigurableLSystem(hilberCurve),
        ConfigurableLSystem(gosperCurve),
        ConfigurableLSystem(sierpinskiTriangle),
        ConfigurableLSystem(sierpinskiArrowheadCurve),
        ConfigurableLSystem(dragonCurve, maxDepth = 14),
        ConfigurableLSystem(fractalPlant)
    )
    var lSystem: ConfigurableLSystem = lSystems.first()

    fun generatePoints(): Sequence<Point> =
        lSystem.value.generatePoints(lSystem.stepLength, lSystem.depth)

    fun switch(direction: Int) {
        val i = lSystems.indexOfFirst { it.value == lSystem.value } + direction
        lSystem = when {
            i < 0 -> lSystems.last()
            i >= lSystems.size -> lSystems.first()
            else -> lSystems[i]
        }
    }

    fun changeDepth(increment: Int) {
        lSystem.depth += increment
        if (lSystem.depth > lSystem.maxDepth) {
            lSystem.depth = lSystem.maxDepth
        }
        if (lSystem.depth <= 0) {
            lSystem.depth = 0
        }
    }

    class ConfigurableLSystem(
        val value: LSystem,
        val maxDepth: Int = 9,
        val url: String? = null
    ) {
        var stepLength: Double = 10.0
        var depth: Int = 1
    }
}

val kochSnowflake = LSystem(
    axiom = "F--F--F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3,
    closedPath = true
)

private val cesaroFractal = LSystem(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = 85.toRadians()
)

// TODO http://mathworld.wolfram.com/CesaroFractal.html
private val cesaroFractal2 = LSystem(
    axiom = "F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3
)

private val quadraticType1Curve = LSystem(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = PI / 2
)

private val quadraticType2Curve = LSystem(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-FF+F+F-F"),
    angle = PI / 2
)

// https://en.wikipedia.org/wiki/Hilbert_curve
private val hilberCurve = LSystem(
    axiom = "A",
    rules = mapOf(
        'A' to "-BF+AFA+FB-",
        'B' to "+AF-BFB-FA+"
    ),
    angle = PI / 2
)

// https://en.wikipedia.org/wiki/Gosper_curve
private val gosperCurve = LSystem(
    axiom = "F",
    rules = mapOf(
        'F' to "F-G--G+F++FF+G-",
        'G' to "+F-GG--G-F++F+G"
    ),
    angle = 60.toRadians()
)

// https://en.wikipedia.org/wiki/Sierpinski_triangle
private val sierpinskiTriangle = LSystem(
    axiom = "F-G-G",
    rules = mapOf(
        'F' to "F-G+F+G-F",
        'G' to "GG"
    ),
    angle = 120.toRadians()
)

/*private val pentaFlake = LSystem(
    axiom = "F-F-F-F-F",
    rules = mapOf(
        'F' to "F[-F-F-F-F]"*//*,
        'G' to "F[-F-F-F-F]"*//*
    ),
    angle = PI / 2.5
)

private val pentaFlake0 = LSystem(
    axiom = "F-F-F-F-F",
    rules = mapOf(
        'F' to "F-F[-F-F-F-F]----F",
        'G' to ""
    ),
    angle = PI / 2.5
)*/

// https://en.wikipedia.org/wiki/Sierpi%C5%84ski_arrowhead_curve
private val sierpinskiArrowheadCurve = LSystem(
    axiom = "F",
    rules = mapOf(
        'F' to "G-F-G",
        'G' to "F+G+F"
    ),
    angle = PI / 3,
    initialAngle = PI
)

// https://en.wikipedia.org/wiki/Dragon_curve
private val dragonCurve = LSystem(
    axiom = "FX",
    rules = mapOf(
        'X' to "X+YF+",
        'Y' to "-FX-Y"
    ),
    angle = PI / 2,
    initialAngle = 1.5 * PI
)

private val fractalPlant = LSystem(
    axiom = "X",
    rules = mapOf(
        'X' to "F[-X][X]F[-X]+FX",
        'F' to "FF"
    ),
    angle = 25.toRadians(),
    initialAngle = -PI / 2
)

// From http://www.cs.unh.edu/~charpov/programming-lsystems.html
private val fractalPlant2 = LSystem(
    axiom = "F",
    rules = mapOf('F' to "FF-[-F+F+F]+[+F-F-F]"),
    angle = 22.5.toRadians(),
    initialAngle = -PI / 2
)


// https://en.wikipedia.org/wiki/L-system
class LSystem(
    var axiom: String,
    var rules: Map<Char, String>,
    var angle: Double,
    val initialAngle: Double = 0.0,
    val closedPath: Boolean = false
) {
    fun generatePoints(stepLength: Double = 10.0, depth: Int = 3): Sequence<Point> {
        return generateOutput(axiom, depth).toPoints(stepLength)
    }

    private fun generateOutput(input: String, depth: Int): String {
        if (depth == 0) return input
        val result = input
            .asIterable()
            .joinToString("") { c -> rules[c] ?: c.toString() }
        return generateOutput(result, depth - 1)
    }

    private fun String.toPoints(stepLength: Double): Sequence<Point> {
        return buildSequence {
            val startPoint = Point(0.0, 0.0)
            yield(startPoint)

            var angle = initialAngle
            var p = startPoint
            val stack = ArrayList<Pair<Point, Double>>()
            forEach { c ->
                when (c) {
                    'F', 'G' -> {
                        p = p.shift(
                            x = cos(angle) * stepLength,
                            y = sin(angle) * stepLength
                        )
                        yield(p)
                    }
                    '+' -> angle += this@LSystem.angle
                    '-' -> angle -= this@LSystem.angle
                    '[' -> stack.add(0, Pair(p, angle))
                    ']' -> {
                        val removed = stack.removeAt(0)
                        p = removed.first
                        angle = removed.second
                        yield(Point.none)
                    }
                }
            }
            if (closedPath) yield(startPoint)
        }
    }
}

fun Double.toDegrees(): Double = (this / PI) * 180
fun Double.toRadians(): Double = (this / 180) * PI
fun Int.toRadians(): Double = toDouble().toRadians()
