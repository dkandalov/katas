@file:Suppress("unused")

package threejs

import org.w3c.dom.Node

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
