package threejs

import lsystem.*
import org.w3c.dom.events.Event
import org.w3c.dom.events.KeyboardEvent
import threejs.THREE.Color
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
    applyTheme1()

    val presenter = LSystemPresenter()

    fun generateScene() {
        scene.clear()

        var geometry = Geometry()
        presenter
            .generatePoints()
            .map { it.copy(y = -it.y) }
            .toList().fitCenteredInto(-100.0, -100.0, 100.0, 100.0)
            .forEach { point ->
                if (point == Point.none) {
                    scene.add(Line(geometry, lineMaterial))
                    geometry = Geometry()
                } else {
                    geometry.vertices.push(Vector3(point.x, point.y, 0.0))
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
    presenter: LSystemPresenter,
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
