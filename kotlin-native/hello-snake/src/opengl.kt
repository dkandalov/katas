import kotlinx.cinterop.*
import platform.GLUT.*
import platform.OpenGL.*
import platform.OpenGLCommon.GLfloat

// Ported from http://openglsamples.sourceforge.net/projects/index.php/blog/index/
class OpenGLWindow {
    private val width = 1280
    private val height = 960
    var rotation: GLfloat = 0.0f
    val rotationSpeed: GLfloat = 0.0f
    var x: GLfloat = 0.0f
    var y: GLfloat = 0.0f
    var points = ArrayList<Point>()

    fun init(f: () -> Unit) {
        window = this

        memScoped {
            val argc = alloc<IntVar>().apply { value = 0 }
            glutInit(argc.ptr, null)
        }

        glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH)

        glutInitWindowSize(this.width, this.height)
        glutCreateWindow("The GLUT Teapot")
        glutDisplayFunc(staticCFunction { ->
//            f()
            display()
        })
        glutIdleFunc(staticCFunction(::display))
        glutKeyboardFunc(staticCFunction(::onKeyPress))
//      glutFullScreen()

        initializeView()

        glutMainLoop()
    }

    private fun initializeView() {
        glMatrixMode(GL_PROJECTION)
        glViewport(0, 0, width, height)
        glMatrixMode(GL_PROJECTION)

        // reset projection matrix
        glLoadIdentity()
        val aspect = width.toDouble() / height

        // set up a perspective projection matrix
        gluPerspective(45.0, aspect, 1.0, 500.0)

        // specify which matrix is the current matrix
        glMatrixMode(GL_MODELVIEW)
        glShadeModel(GL_SMOOTH)

        // specify the clear value for the depth buffer
        glClearDepth(1.0)
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LEQUAL)

        // specify implementation-specific hints
        glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)

        glLightModelfv(GL_LIGHT_MODEL_AMBIENT, cValuesOf(0.1f, 0.1f, 0.1f, 1.0f))
        glLightfv(GL_LIGHT0, GL_DIFFUSE, cValuesOf(0.6f, 0.6f, 0.6f, 1.0f))
        glLightfv(GL_LIGHT0, GL_SPECULAR, cValuesOf(0.7f, 0.7f, 0.3f, 1.0f))

        glEnable(GL_LIGHT0)
        glEnable(GL_COLOR_MATERIAL)
        glShadeModel(GL_SMOOTH)
        glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE)
        glDepthFunc(GL_LEQUAL)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glClearColor(0.0f, 0.0f, 0.0f, 1.0f)
    }

    fun cube(x: Int, y: Int) {
        points.add(Point(x, y))
    }

    fun clear(x: Int, y: Int) {
        points.remove(Point(x, y))
    }
}

private lateinit var window: OpenGLWindow

private fun display() {
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()

    // Define a viewing transformation
    gluLookAt(4.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)

    // Push and pop the current matrix stack.
    // This causes that translations and rotations on this matrix wont influence others.

    glPushMatrix()
    glColor3f(1.0f, 0.0f, 0.0f)
    glTranslatef(window.x, window.y, 0.0f)
    glRotatef(window.rotation, 0.0f, 1.0f, 0.0f)
    glRotatef(90.0f, 0.0f, 1.0f, 0.0f)

//    window.points.forEach { point ->
//        glPushMatrix()
//        glTranslatef(point.x.toFloat(), point.y.toFloat(), 0.0f)
//        glutSolidCube(0.1)
//        glPopMatrix()
//    }
    glutSolidCube(0.1)

    glPopMatrix()

    window.rotation += window.rotationSpeed
    glutSwapBuffers()
}


@Suppress("UNUSED_PARAMETER")
private fun onKeyPress(char: Byte, _x: Int, _y: Int) {
    when (char.toChar()) {
        'd' -> window.x += 0.2f
        'a' -> window.x -= 0.2f
        'w' -> window.y += 0.2f
        's' -> window.y -= 0.2f
    }
}