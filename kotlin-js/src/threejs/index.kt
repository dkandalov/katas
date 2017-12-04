package threejs

import lsystem.inputById
import lsystem.toDegrees
import lsystem.toRadians
import lsystem.toggleConfigToolbar
import org.w3c.dom.events.Event
import org.w3c.dom.events.KeyboardEvent
import threejs.THREE.Color
import threejs.THREE.Euler
import threejs.THREE.Geometry
import threejs.THREE.Line
import threejs.THREE.LineBasicMaterial
import threejs.THREE.OrbitControls
import threejs.THREE.PerspectiveCamera
import threejs.THREE.Scene
import threejs.THREE.Vector3
import threejs.THREE.WebGLRenderer
import kotlin.browser.document
import kotlin.browser.window
import kotlin.coroutines.experimental.buildSequence
import kotlin.math.PI
import kotlin.math.min

/**
 * Translation of https://github.com/mrdoob/three.js/blob/334ab72b4251f5dd0abc5c72a96942d438eae24a/examples/webgl_lines_cubes.html
 */
fun main() {
    init()
    animate()
}

lateinit var camera: PerspectiveCamera
lateinit var scene: Scene
lateinit var renderer: WebGLRenderer
var windowHalfX = window.innerWidth / 2.0
var windowHalfY = window.innerHeight / 2.0

val material1 = LineBasicMaterial(object {}.applyDynamic {
    color = 0x000000
    linewidth = 5.0
    opacity = 1.0
    blending = THREE.AdditiveBlending
    transparent = false
})
val material2 = LineBasicMaterial(object {}.applyDynamic {
    color = 0xFFFFFF
    opacity = 1.0
    blending = THREE.AdditiveBlending
    transparent = false
})
var lineMaterial = material1


fun init() {
    val container = document.createElement("div")
    document.body?.appendChild(container)

    camera = PerspectiveCamera(
        fov = 33.0,
        aspect = window.innerWidth.toDouble() / window.innerHeight,
        near = 1.0,
        far = 10000.0
    )
    camera.position.set(0, 0, 400)

    scene = Scene()
    renderer = WebGLRenderer().apply {
        setPixelRatio(window.devicePixelRatio)
        setSize(window.innerWidth, window.innerHeight)
        container.appendChild(this.domElement)
    }
    applyTheme2()

    val presenter = LSystem3dPresenter()

    fun generateScene() {
        scene.clear()

        var geometry = Geometry()
        presenter
            .generatePoints()
            .toList().fitCenteredInto(-100.0, -100.0, -100.0, 100.0, 100.0, 100.0)
//            .onEach { println(it.x.toString() + " " + it.y + " " + it.z) }
            .forEach {
                if (it === LSystem3d.emptyVector) {
                    scene.add(Line(geometry, lineMaterial))
                    geometry = Geometry()
                } else {
                    geometry.vertices.push(it)
                }
            }
        if (geometry.vertices.length > 0) {
            scene.add(Line(geometry, lineMaterial))
        }

        render()
    }
    generateScene()

    val orbitControls = OrbitControls(camera, renderer.domElement)
    orbitControls.keyPanSpeed = 0.0

    initConfigToolbar(presenter, ::generateScene)
    updateConfigToolbar(presenter)

    window.addEventListener("resize", ::onWindowResize, false)
    window.addEventListener("keypress", onKeyPress(presenter, orbitControls, ::generateScene))
}

private fun THREE.Object3D.clear() {
    while (children.length > 0) {
        val children: dynamic = children
        remove(children[0])
    }
}

private fun onKeyPress(
    presenter: LSystem3dPresenter,
    orbitControls: OrbitControls,
    updateUI: () -> Unit
): (Event) -> Unit {
    val mapping = mapOf(
        "n" to { presenter.switch(1) },
        "N" to { presenter.switch(-1) },
        "d" to { presenter.changeDepth(1) },
        "D" to { presenter.changeDepth(-1) },
        "t" to { toggleConfigToolbar(document) },
        "c" to { orbitControls.reset() },
        "q" to { applyTheme1() },
        "w" to { applyTheme2() }
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

private fun initConfigToolbar(presenter: LSystem3dPresenter, updateUI: () -> Unit) {
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

private fun updateConfigToolbar(presenter: LSystem3dPresenter) {
    inputById("axiom").value = presenter.lSystem.value.axiom
    inputById("rules").value = presenter.lSystem.value.rules
        .entries.joinToString("; ") { it.key + " => " + it.value }
    inputById("angle").value = presenter.lSystem.value.angle.toDegrees().toString()
}

private fun applyTheme1() {
    lineMaterial = material1
    scene.background = Color(0xffffff)
    document.body?.style?.background = "#ffffff"
}

private fun applyTheme2() {
    lineMaterial = material2
    scene.background = Color(0x000000)
    document.body?.style?.background = "#000000"
}

@Suppress("UNUSED_PARAMETER")
private fun animate(d: Double = 0.0) {
    window.requestAnimationFrame(::animate)
    render()
}

private fun render() {
    renderer.render(scene, camera)
}

@Suppress("UNUSED_PARAMETER")
private fun onWindowResize(event: Event) {
    windowHalfX = window.innerWidth / 2.0
    windowHalfY = window.innerHeight / 2.0

    camera.aspect = window.innerWidth / window.innerHeight.toDouble()
    camera.updateProjectionMatrix()

    renderer.setSize(window.innerWidth, window.innerHeight)
}

private fun List<Vector3>.fitCenteredInto(x1: Double, y1: Double, z1: Double, x2: Double, y2: Double, z2: Double): List<Vector3> {
    require(x1 < x2 && y1 < y2 && z1 < z2)
    val width = x2 - x1
    val height = y2 - y1
    val depth = z2 - z1

    val minPoint = Vector3(minBy{ it.x }!!.x, minBy{ it.y }!!.y, minBy{ it.z }!!.z)
    val maxPoint = Vector3(maxBy{ it.x }!!.x, maxBy{ it.y }!!.y, maxBy{ it.z }!!.z)
    val pointsWidth = maxPoint.x - minPoint.x
    val pointsHeight = maxPoint.y - minPoint.y
    val pointsDepth = maxPoint.z - minPoint.z
    val minScale = min(min(width / pointsWidth, height / pointsHeight), depth / pointsDepth)

    return this
        .map {
            it.multiplyScalar(minScale)
            it.set(
                x = it.x + x1 - minPoint.x * minScale + (width - pointsWidth * minScale) / 2,
                y = it.y + y1 - minPoint.y * minScale + (height - pointsHeight * minScale) / 2,
                z = it.z + z1 - minPoint.z * minScale + (depth - pointsDepth * minScale) / 2
            )
            it
        }
}


class LSystem3dPresenter {
    val lSystems = listOf(
//        ConfigurableLSystem(hilbertCurve3d),
//        ConfigurableLSystem(kochCurve3d),
        ConfigurableLSystem(kochSnowflake),
        ConfigurableLSystem(cesaroFractal),
        ConfigurableLSystem(quadraticType2Curve),
        ConfigurableLSystem(quadraticType1Curve),
        ConfigurableLSystem(hilbertCurve),
        ConfigurableLSystem(gosperCurve),
        ConfigurableLSystem(sierpinskiTriangle),
        ConfigurableLSystem(sierpinskiArrowheadCurve),
        ConfigurableLSystem(dragonCurve, maxDepth = 14),
        ConfigurableLSystem(fractalPlant)
    )
    var lSystem: ConfigurableLSystem = lSystems.first()

    fun generatePoints(): Sequence<Vector3> =
        lSystem.value.generatePoints(lSystem.depth)

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
        val value: LSystem3d,
        val maxDepth: Int = 9,
        val url: String? = null
    ) {
        var depth: Int = 1
    }
}

private val kochSnowflake = LSystem3d(
    axiom = "F--F--F",
    rules = mapOf('F' to "F+F--F+F"),
    angle = PI / 3,
    closedPath = true
)

private val cesaroFractal = LSystem3d(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = 85.toRadians()
)

private val quadraticType1Curve = LSystem3d(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-F+F"),
    angle = PI / 2
)

private val quadraticType2Curve = LSystem3d(
    axiom = "F",
    rules = mapOf('F' to "F+F-F-FF+F+F-F"),
    angle = PI / 2
)

// https://en.wikipedia.org/wiki/Hilbert_curve
private val hilbertCurve = LSystem3d(
    axiom = "A",
    rules = mapOf(
        'A' to "-BF+AFA+FB-",
        'B' to "+AF-BFB-FA+"
    ),
    angle = PI / 2
)

// https://en.wikipedia.org/wiki/Gosper_curve
private val gosperCurve = LSystem3d(
    axiom = "F",
    rules = mapOf(
        'F' to "F-G--G+F++FF+G-",
        'G' to "+F-GG--G-F++F+G"
    ),
    angle = 60.toRadians()
)

// https://en.wikipedia.org/wiki/Sierpinski_triangle
private val sierpinskiTriangle = LSystem3d(
    axiom = "F-G-G",
    rules = mapOf(
        'F' to "F-G+F+G-F",
        'G' to "GG"
    ),
    angle = 120.toRadians()
)

// https://en.wikipedia.org/wiki/Sierpi%C5%84ski_arrowhead_curve
private val sierpinskiArrowheadCurve = LSystem3d(
    axiom = "F",
    rules = mapOf(
        'F' to "G-F-G",
        'G' to "F+G+F"
    ),
    angle = PI / 3
)

// https://en.wikipedia.org/wiki/Dragon_curve
private val dragonCurve = LSystem3d(
    axiom = "FX",
    rules = mapOf(
        'X' to "X+YF+",
        'Y' to "-FX-Y"
    ),
    angle = PI / 2
)

private val fractalPlant = LSystem3d(
    axiom = "X",
    rules = mapOf(
        'X' to "F[-X][X]F[-X]+FX",
        'F' to "FF"
    ),
    angle = 25.toRadians()
)

private val hilbertCurve3d = LSystem3d(
    axiom = "X",
    rules = mapOf('X' to "^<XF^<XFX-F^>>XFX&F+>>XFX-F>X->"),
    angle = PI / 2
)

private val kochCurve3d = LSystem3d(
    axiom = "A",
    rules = mapOf(
        'A' to "[[[[F+F-F-F+F]G<G>G>G<G]H-H+H+H-H]I>I<I<I>I]",
        'F' to "F+F-F-F+F",
        'G' to "G<G>G>G<G",
        'H' to "H-H+H+H-H",
        'I' to "I>I<I<I>I"
    ),
    angle = PI / 2
)

class LSystem3d(
    var axiom: String,
    var rules: Map<Char, String>,
    var angle: Double,
    val closedPath: Boolean = false,
    val stepLength: Double = 10.0
) {
    fun generatePoints(depth: Int = 3): Sequence<Vector3> {
        return generateOutput(axiom, depth).toPoints(stepLength)
    }

    private fun generateOutput(input: String, depth: Int): String {
        if (depth == 0) return input
        val result = input
            .asIterable()
            .joinToString("") { char ->
                rules[char] ?: char.toString()
            }
        return generateOutput(result, depth - 1)
    }

    private fun String.toPoints(stepLength: Double): Sequence<Vector3> {
        return buildSequence {
            val startPoint = Vector3(0, 0, 0)
            yield(startPoint.clone())

            var angles = Vector3(0, 0, 0)
            var p = startPoint.clone()
            val stack = ArrayList<Pair<Vector3, Vector3>>()
            forEach { c ->
                when (c) {
                    'F', 'G', 'H', 'I' -> {
                        val v = Vector3(0, stepLength, 0)
                        v.applyEuler(Euler(angles.x, angles.y, angles.z, "XYZ"))
                        p.add(v)
                        yield(p.clone())
                    }

                    '+' -> angles.z += this@LSystem3d.angle
                    '-' -> angles.z -= this@LSystem3d.angle

                    '<' -> angles.x += this@LSystem3d.angle
                    '>' -> angles.x -= this@LSystem3d.angle
                    '|' -> angles.x -= this@LSystem3d.angle * 2

                    '^' -> angles.y += this@LSystem3d.angle
                    '&' -> angles.y -= this@LSystem3d.angle

                    '[' -> stack.add(0, Pair(p.clone(), angles.clone()))
                    ']' -> {
                        val removed = stack.removeAt(0)
                        p = removed.first
                        angles = removed.second
                        yield(emptyVector)
                    }
                }
            }
            if (closedPath) yield(startPoint)
        }
    }

    companion object {
        val emptyVector = Vector3(Double.NaN, Double.NaN, Double.NaN)
    }
}
