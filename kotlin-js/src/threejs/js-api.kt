@file:Suppress("unused")

package threejs

import org.w3c.dom.Node

external object THREE {
    class PerspectiveCamera(fov: Number, aspect: Double, near: Number, far: Number): Camera {
        var aspect: Double
        fun updateProjectionMatrix()
        fun lookAt(position: Vector3)
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    class Scene: Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
        var background: Color
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
    class BufferGeometry

    interface Camera: Object3D

    interface Object3D {
        val scale: Vector3
        val position: Vector3
        val children: JsArray<Object3D>
        fun add(object3D: Object3D)
        fun remove(object3D: Object3D)
    }

    class Vector3(x: Number, y: Number, z: Number) {
        var x: Double
        var y: Double
        var z: Double
        fun set(x: Number, y: Number, z: Number)
    }

    class LineBasicMaterial(any: dynamic)
    class MeshBasicMaterial(any: dynamic)

    class Line(geometry: Geometry, material: LineBasicMaterial): Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    val NormalBlending: Int
    val AdditiveBlending: Int
    val SubtractiveBlending: Int
    val MultiplyBlending: Int
    val CustomBlending: Int

    class CubeGeometry(width: Double, height: Double, depth: Double)

    class Mesh(cubeGeometry: CubeGeometry, meshBasicMaterial: MeshBasicMaterial): Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    class SpotLight(color: Int, intensity: Double): Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    class AmbientLight(color: Int): Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    class OrbitControls(camera: Camera, element: Node) {
        fun reset()
        var keyPanSpeed: Double
    }

    class AxisHelper(size: Int): Object3D {
        override val scale: Vector3
        override val position: Vector3
        override val children: JsArray<Object3D>
        override fun add(object3D: Object3D)
        override fun remove(object3D: Object3D)
    }

    class Color(value: Any)
}

@JsName("Array")
external class JsArray<T> {
    val length: Int
    fun push(item: T)
    fun pop(): T
    operator fun get(index: Int): T
    fun set(index: Int, value: T)
}

fun <T> T.applyDynamic(f: dynamic.() -> Unit): T {
    f(this.asDynamic())
    return this
}
