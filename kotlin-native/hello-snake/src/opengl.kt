import kotlinx.cinterop.*
import platform.GLUT.*
import platform.OpenGL.*
import platform.OpenGLCommon.GLfloat

// Ported from http://openglsamples.sourceforge.net/projects/index.php/blog/index/
class OpenGLWindow {

    fun init() {
        memScoped {
            val argc = alloc<IntVar>().apply { value = 0 }
            glutInit(argc.ptr, null)
        }

        glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH)

        glutInitWindowSize(windowWidth, windowHeight)
        glutCreateWindow("The GLUT Teapot")
        glutDisplayFunc(staticCFunction(::display))
        glutIdleFunc(staticCFunction(::display))
        glutKeyboardFunc(staticCFunction(::onKeyPress))
//      glutFullScreen()

        initializeView()

        glutMainLoop()
    }

    private fun initializeView() {
        // select projection matrix
        glMatrixMode(GL_PROJECTION)

        // set the viewport
        glViewport(0, 0, windowWidth, windowHeight)

        // set matrix mode
        glMatrixMode(GL_PROJECTION)

        // reset projection matrix
        glLoadIdentity()
        val aspect = windowWidth.toDouble() / windowHeight

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

}

private var rotation: GLfloat = 0.0f
private val rotationSpeed: GLfloat = 0.2f
private var x: GLfloat = 0.0f
private var y: GLfloat = 0.0f

private val windowWidth = 1280
private val windowHeight = 960


fun display() {
    // Clear Screen and Depth Buffer
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()

    // Define a viewing transformation
    gluLookAt(4.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)

    // Push and pop the current matrix stack.
    // This causes that translations and rotations on this matrix wont influence others.

    glPushMatrix()
    glColor3f(1.0f, 0.0f, 0.0f)
    glTranslatef(x, y, 0.0f)
    glRotatef(rotation, 0.0f, 1.0f, 0.0f)
    glRotatef(90.0f, 0.0f, 1.0f, 0.0f)

//    glutSolidTeapot(1.0)
    glutSolidCube(1.0)
    glPopMatrix()


    rotation += rotationSpeed
    glutSwapBuffers()
}


@Suppress("UNUSED_PARAMETER")
fun onKeyPress(char: Byte, _x: Int, _y: Int) {
    when (char.toChar()) {
        'd' -> x += 0.2f
        'a' -> x -= 0.2f
        'w' -> y += 0.2f
        's' -> y -= 0.2f
    }
}