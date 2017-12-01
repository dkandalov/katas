package threejs

import org.w3c.dom.Node
import org.w3c.dom.events.Event
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

fun init() {
    val container = document.createElement("div")
    document.body?.appendChild(container)

    camera = PerspectiveCamera(
        fov = 33.0,
        aspect = window.innerWidth / window.innerHeight.toDouble(),
        near = 1.0,
        far = 10000.0
    )
    camera.position.z = 700.0

    scene = Scene()
    renderer = WebGLRenderer()
    renderer.setPixelRatio(window.devicePixelRatio)
    renderer.setSize(window.innerWidth, window.innerHeight)
    container.appendChild(renderer.domElement)
    
    val geometry = THREE.Geometry()
    val points = listOf(
        Vector3(-193.75, 193.75, -193.75),
        Vector3(-193.75, 181.25, -193.75),
        Vector3(-181.25, 181.25, -193.75),
        Vector3(-181.25, 193.75, -193.75)
    )
    points.forEach {
        geometry.vertices.push(it)
    }

    val material = LineBasicMaterial(object {}.applyDynamic {
        color = 0x553300
        opacity = 1.0
        blending = THREE.AdditiveBlending
        transparent = true
    })

    points.forEach { point ->
        val line = THREE.Line(geometry, material)
        line.scale.apply {
            x = 0.15
            y = 0.15
            z = 0.15
        }
        line.position.apply {
            x = point.x
            y = point.y
            z = point.z
        }
        scene.add(line)
    }

    window.addEventListener("resize", ::onWindowResize, false)
}

fun animate(d: Double = 0.0) {
    window.requestAnimationFrame(::animate)
    render()
}

fun render() {
    camera.position.x += (mouseX - camera.position.x) * .05
    camera.position.y += (-mouseY + 200 - camera.position.y) * .05
    camera.lookAt(scene.position)

    // TODO
}

fun onWindowResize(event: Event) {
    camera.aspect = window.innerWidth / window.innerHeight.toDouble()
    camera.updateProjectionMatrix()
    renderer.setSize(window.innerWidth, window.innerHeight)
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

    class LineBasicMaterial(any: Any) {
        companion object {
            class Parameters {
                var color: dynamic
                var opacity: dynamic
                var blending: dynamic
                var transparent: Boolean
            }
        }
    }

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
