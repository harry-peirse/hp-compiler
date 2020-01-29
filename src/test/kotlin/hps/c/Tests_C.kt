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
        return Stream.of(// args[0]: expected result code, args[1]: expected standard out string, args[2]: code to compile
            Arguments.of(1, "", "main :: int { return 1; }"),
            Arguments.of(2, "", "main :: int { if(1) return 2; else return 3; }"),
            Arguments.of(3, "", "main :: int { if(0) return 2; else return 3; }"),
            Arguments.of(4, "", "main :: int { int a = 4; return a; }"),
            Arguments.of(5, "", "main :: int { int a = 4; a += 1; return a; }"),
            Arguments.of(6, "", "main :: int { int a = 4; { int a = 2; } a = a + 2; return a; }"),
            Arguments.of(7, "", "main :: int { int a = 4; { a = 5; } a = a + 2; return a; }")
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
    fun testParameters(expectedStatus: Int, expectedStdOut: String, inputString: String, @TempDir tempDir: Path) {
        val int = atomicInt.incrementAndGet()
        File("./build/test$int.hp").writeText(inputString)
        val output = compileAndRun("./build/test$int.hp", tempDir)
        assertEquals(expectedStatus, output.first)
        assertEquals(expectedStdOut, output.second)
    }

    @TestFactory
    fun testFiles(@TempDir tempDir: Path) = fileSource()?.map { file ->
        DynamicTest.dynamicTest(file.path) {
            val output = compileAndRun(file.path, tempDir)
            val expectedStatus = file.path.split("_").last().split(".").first().toInt()
            val expectedStdOut = file.name.split("_")[1]
            assertEquals(expectedStatus, output.first)
            assertEquals(expectedStdOut, output.second)
        }
    }

    private fun compileAndRun(filename: String, outputPath: Path): Pair<Int, String> {
        val tokens = Lexer().lex(filename)
        println(tokens.joinToString("\n") { it.prettyPrint() })
        val parser = Ast()
        val program = parser.parseProgram(tokens)
        val c = Validator().validate(program)
        var lineNumber = 1
        println(c.split("\n").joinToString("\n") { "%4d    $it".format(lineNumber++) } + "\n")

        val fileNameWithoutExtension =
            outputPath.toString() + '\\' + filename.removeSuffix(".hp").split("\\").last().split("/").last()
        val cFileName = "$fileNameWithoutExtension.c"
        val executableFileName = "$fileNameWithoutExtension.exe"
        println(executableFileName)

        File(cFileName).takeIf { it.exists() }?.delete()
        File(cFileName).writeText(c)

        File(executableFileName).takeIf { it.exists() }?.delete()
        val gccOutput = runProcess("gcc", "-g", cFileName, "-o", fileNameWithoutExtension)
        println("GCC exit status is $gccOutput\n")

        val runtimeOutput = runProcess(executableFileName)
        println("\nOutput is: $runtimeOutput\n")

        return runtimeOutput
    }

    private fun runProcess(vararg args: String): Pair<Int, String> {
        val processBuilder = ProcessBuilder(args.asList())
        processBuilder.redirectErrorStream(true)
        processBuilder.redirectOutput(ProcessBuilder.Redirect.PIPE)

        val process = processBuilder.start()
        val bufferedReader = BufferedReader(InputStreamReader(process.inputStream))
        var output = ""
        var line = bufferedReader.readLine()
        while (line != null) {
            output += line
            println(line)
            line = bufferedReader.readLine()
        }

        return process.waitFor() to output
    }
}