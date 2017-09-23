package katas.kotlin.dastahkay

import org.junit.Test
import java.io.File
import kotlin.test.currentStackTrace

class ApprovalExampleTest {

    @Test fun `1 plus 2`() {
        assertApproved(1 + 3)
    }

    private fun assertApproved(actualValue: Any?) {
        assertApproved(actualValue.toString())
    }

    private fun assertApproved(actualValue: String) {
        val method = currentStackTrace()
            .asSequence()
            .map { stackTraceElement ->
                val aClass = Class.forName(stackTraceElement.className)
                aClass.methods.find { method ->
                    method.name == stackTraceElement.methodName
                        && method.declaredAnnotations.any { it.annotationClass == Test::class }
                }
            }
            .filterNotNull()
            .firstOrNull() ?: error("Couldn't find method with @Test annotation.")

        val srcPath = "src" + File.separatorChar
        val packageDirPath = srcPath + method.declaringClass.`package`.name.replace('.', File.separatorChar) + File.separatorChar
        val packageDir = File(packageDirPath)
        packageDir.mkdirs()

        val fileName = method.declaringClass.simpleName + "." + method.name
        val approvedFile = File(packageDirPath + fileName + ".approved")
        val actualFile = File(packageDirPath + fileName + ".actual")

        if (approvedFile.exists()) {
            val approvedValue = approvedFile.readText()
            if (approvedValue != actualValue) {
                if (!actualFile.exists()) {
                    val wasCreated = actualFile.createNewFile()
                    if (!wasCreated) error("Failed to create $actualFile")
                }
                actualFile.writeText(actualValue)
                throw AssertionError("Expected a value $approvedValue but it was $actualValue")
            }
        } else {
            val wasCreated = approvedFile.createNewFile()
            if (!wasCreated) error("Failed to create $approvedFile")
            approvedFile.writeText(actualValue)
        }
        println(method.declaringClass.simpleName + "." + method.name + ".approved")
    }
}