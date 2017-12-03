package threejs

import lsystem.fitCenteredInto
import lsystem.kochSnowflake
import org.w3c.dom.events.Event
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

fun init() {
    val container = document.createElement("div")
    document.body?.appendChild(container)

    camera = PerspectiveCamera(
        fov = 33.0,
        aspect = window.innerWidth / window.innerHeight.toDouble(),
        near = 1.0,
        far = 10000.0
    )
    camera.position.z = 200.0

    scene = Scene()
    renderer = WebGLRenderer().apply {
        setPixelRatio(window.devicePixelRatio)
        setSize(window.innerWidth, window.innerHeight)
        container.appendChild(this.domElement)
    }

    val geometry = THREE.Geometry()
    kochSnowflake
        .generatePoints()
        .toList().fitCenteredInto(-100.0, -100.0, 100.0, 100.0)
        .map { Vector3(it.x, it.y, 0.0) }
        .forEach {
            geometry.vertices.push(it)
        }
    val material = LineBasicMaterial(object {}.applyDynamic {
        color = 0xFFFFFF
        opacity = 1.0
        blending = THREE.AdditiveBlending
        transparent = true
    })
    val line = THREE.Line(geometry, material).apply {
        scale.set(0.5, 0.5, 0.5)
        position.set(0, 0, 0)
    }
    scene.add(line)

    OrbitControls(camera, renderer.domElement)

    window.addEventListener("resize", ::onWindowResize, false)
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
