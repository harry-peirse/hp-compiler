package hps.c

import hps.Lexer
import hps.atomicInt
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.api.parallel.Execution
import org.junit.jupiter.api.parallel.ExecutionMode
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsProvider
import org.junit.jupiter.params.provider.ArgumentsSource
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.nio.file.Path
import java.util.stream.Stream

class TestArgumentProvider : ArgumentsProvider {
    override fun provideArguments(context: ExtensionContext?): Stream<out Arguments> {
        return Stream.of(
            Arguments.of(1, "int main() { return 1; }"),
            Arguments.of(2, "int main() { if(1) return 2; else return 3; }"),
            Arguments.of(3, "int main() { if(0) return 2; else return 3; }"),
            Arguments.of(4, "int main() { int a = 4; return a; }"),
            Arguments.of(5, "int main() { int a = 4; a += 1; return a; }"),
            Arguments.of(6, "int main() { int a = 4; { int a = 2; } a = a + 2; return a; }"),
            Arguments.of(7, "int main() { int a = 4; { a = 5; } a = a + 2; return a; }")
        )
    }
}

@Execution(ExecutionMode.CONCURRENT)
class Tests {

    private fun fileSource() = File("./src/test/resources")
        .listFiles()?.filter {
            it.name.endsWith(".hp")
        }

    @ParameterizedTest
    @ArgumentsSource(TestArgumentProvider::class)
    fun testParameters(expectedOutput: Int, inputString: String, @TempDir tempDir: Path) {
        val int = atomicInt.incrementAndGet()
        File("./build/test$int.hp").writeText(inputString)
        val output = compileAndRun("./build/test$int.hp", tempDir)
        assertEquals(expectedOutput, output)
    }

    @TestFactory
    fun testFiles(@TempDir tempDir: Path) = fileSource()?.map { file ->
        DynamicTest.dynamicTest(file.path) {
            val resultStatus = compileAndRun(file.path, tempDir)
            val expectedStatus = file.path.split("_").last().split(".").first().toInt()
            assertEquals(expectedStatus, resultStatus)
        }
    }

    private fun compileAndRun(filename: String, outputPath: Path): Int {
        println("Program input:")
        println(File(filename).readText() + "\n")

        val tokens = Lexer().lex(filename)
        println("Lexical Analysis:")
        println(tokens.joinToString("\n") { it.prettyPrint() } + "\n")

        val parser = Ast()
        val ast = parser.parseProgram(tokens)
        println("Parsed AST:")
        println("${ast}\n")

        val c = ast.toC()
        println("Generated C:")
        var lineNumber = 1
        println(c.split("\n").joinToString("\n") { "%3d $it".format(lineNumber++) } + "\n")

        val fileNameWithoutExtension =
            outputPath.toString() + '\\' + filename.removeSuffix(".hp").split("\\").last().split("/").last()
        val cFileName = "$fileNameWithoutExtension.c"
        val executableFileName = "$fileNameWithoutExtension.exe"
        println(executableFileName)

        File(cFileName).takeIf { it.exists() }?.delete()
        File(cFileName).writeText(c)

        File(executableFileName).takeIf { it.exists() }?.delete()
        val processBuilder1 = ProcessBuilder("gcc", "-m32", "-g", cFileName, "-o", fileNameWithoutExtension)
        processBuilder1.redirectErrorStream(true)
        val process1 = processBuilder1.start()
        val bufferedReader1 = BufferedReader(InputStreamReader(process1.inputStream))
        var line1 = bufferedReader1.readLine()
        while (line1 != null) {
            println(line1)
            line1 = bufferedReader1.readLine()
        }
        println("GCC exit status is ${process1.waitFor()}\n")

        val processBuilder2 = ProcessBuilder(executableFileName)
        processBuilder2.redirectErrorStream(true)

        processBuilder2.redirectOutput(ProcessBuilder.Redirect.INHERIT)
        val process2 = processBuilder2.start()
        val bufferedReader2 = BufferedReader(InputStreamReader(process1.inputStream))
        var line2 = bufferedReader2.readLine()
        while (line2 != null) {
            println(line2)
            line2 = bufferedReader2.readLine()
        }

        val status = process2.waitFor()
        println("\nOutput is: $status\n")

        return status
    }
}