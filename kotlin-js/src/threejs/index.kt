package threejs

import lsystem.kochSnowflake
import org.w3c.dom.Node
import org.w3c.dom.events.Event
import org.w3c.dom.events.MouseEvent
import threejs.THREE.LineBasicMaterial
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
var mouseX = 0.0
var mouseY = 0.0
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
    camera.position.z = 1000.0

    scene = Scene()
    renderer = WebGLRenderer()
    renderer.setPixelRatio(window.devicePixelRatio)
    renderer.setSize(window.innerWidth, window.innerHeight)
    container.appendChild(renderer.domElement)

    val points = kochSnowflake
        .generatePoints(stepLength = 20.0)
        .map { Vector3(it.x, it.y, -200.0) }

//    val points = listOf(
//        Vector3(-193.75, 193.75, -200.0),
//        Vector3(-193.75, 181.25, -200.0),
//        Vector3(-181.25, 181.25, -200.0),
//        Vector3(-181.25, 193.75, -200.0)
//    )
    val geometry = THREE.Geometry()
    points.forEach {
        geometry.vertices.push(it)
    }

    val material = LineBasicMaterial(object {}.applyDynamic {
        color = 0xFFFFFF
        opacity = 1.0
        blending = THREE.AdditiveBlending
        transparent = true
    })

    val line = THREE.Line(geometry, material)
    line.scale.apply {
        x = 0.5
        y = 0.5
        z = 0.5
    }
    line.position.apply {
        x = points.first().x
        y = points.first().y
        z = points.first().z
    }
    scene.add(line)

    document.addEventListener("mousemove", ::onDocumentMouseMove, false)
    window.addEventListener("resize", ::onWindowResize, false)
}

@Suppress("UNUSED_PARAMETER")
fun animate(d: Double = 0.0) {
    window.requestAnimationFrame(::animate)
    render()
}

fun render() {
    camera.position.x += (mouseX - camera.position.x) * .05
    camera.position.y += (-mouseY + 200 - camera.position.y) * .05
    camera.lookAt(scene.position)
    renderer.render(scene, camera)
}

@Suppress("UNUSED_PARAMETER")
fun onWindowResize(event: Event) {
    windowHalfX = window.innerWidth / 2.0
    windowHalfY = window.innerHeight / 2.0

    camera.aspect = window.innerWidth / window.innerHeight.toDouble()
    camera.updateProjectionMatrix()

    renderer.setSize(window.innerWidth, window.innerHeight)
}

fun onDocumentMouseMove(event: Event) {
    if (event is MouseEvent) {
        mouseX = event.clientX - windowHalfX
        mouseY = event.clientY - windowHalfY
    }
}

external object THREE {
    class PerspectiveCamera(fov: Double, aspect: Double, near: Double, far: Double): Camera {
        override val scale: Vector3
        override val position: Vector3
        var aspect: Double
        fun updateProjectionMatrix()
        fun lookAt(position: Vector3)
    }

    class Scene: Object3D {
        override val scale: Vector3
        override val position: Vector3
        fun add(line: Object3D)
    }

    class WebGLRenderer {
        val domElement: Node
        fun setPixelRatio(ratio: Double)
        fun setSize(width: Int, height: Int)
        fun render(scene: Scene, camera: PerspectiveCamera)
    }

    class Geometry {
        val vertices: JsArray<Vector3>
    }

    interface Camera: Object3D

    interface Object3D {
        val scale: Vector3
        val position: Vector3
    }

    class Vector3(x: Double, y: Double, z: Double) {
        var x: Double
        var y: Double
        var z: Double
    }

    class LineBasicMaterial(any: Any)

    class Line(geometry: Geometry, material: LineBasicMaterial): Object3D {
        override val scale: Vector3
        override val position: Vector3
    }

    val NormalBlending: Int
    val AdditiveBlending: Int
    val SubtractiveBlending: Int
    val MultiplyBlending: Int
    val CustomBlending: Int
}

@JsName("Array")
external class JsArray<T> {
    fun push(item: T)
    fun pop(): T
    fun get(index: Int): T
    fun set(index: Int, value: T)
}

fun <T> T.applyDynamic(f: dynamic.() -> Unit): T {
    f(this.asDynamic())
    return this
}
