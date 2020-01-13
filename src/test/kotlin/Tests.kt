package com.aal.hp

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.extension.ExtensionContext
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.ArgumentsProvider
import org.junit.jupiter.params.provider.ArgumentsSource
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.stream.Stream


class TestArgumentProvider : ArgumentsProvider {
    override fun provideArguments(context: ExtensionContext?): Stream<out Arguments> {
        return Stream.of(
            Arguments.of(2, "int main() { return 2; }"),
            Arguments.of(3, "int main() { return 1 + 2; }"),
            Arguments.of(7, "int main() { return 1 + 2 * 3; }"),
            Arguments.of(13, "int main() { return 2 * 2 + 3 * 3; }"),
            Arguments.of(30, "int main() { return 2 * (2 + 3) * 3; }"),
            Arguments.of(43, "int main() { return 5 / 4 + 6 * 7; }"),
            Arguments.of(38, "int main() { return -3 - 5 / 4 + 6 * 7; }"),
            Arguments.of(35, "int main() { return 2 * -3 - 5 / 4 + 6 * 7; }"),
            Arguments.of(47, "int main() { return 2 * 3 - 5 / 4 + 6 * 7; }"),
            Arguments.of(36, "int main() { return 1 + 2 * -3 - 5 / 4 + 6 * 7; }"),
            Arguments.of(1, "int main() { return 1 == 1; }"),
            Arguments.of(0, "int main() { return 1 == 2; }"),
            Arguments.of(1, "int main() { return 0 || 1; }"),
            Arguments.of(1, "int main() { return 1 || 1; }"),
            Arguments.of(0, "int main() { return 0 || 0; }"),
            Arguments.of(3, "int main() { int a = 1; return a + 2; }"),
            Arguments.of(3, "int main() { int a = 1; int b = 2; return a + b; }"),
            Arguments.of(2, "int main() { int a = 1; a = 2; return a; }"),
            Arguments.of(4, "int main() { int a; a = 4; return a; }"),
            Arguments.of(1, "int main() { int a = 2; if(a > 1) a = 1; return a; }"),
            Arguments.of(4, "int main() { int a; if(0) a = 1; else a = 2; return a; }"),
            Arguments.of(3, "int main() { return 0 || 1 ? 3 : 5; }"),
            Arguments.of(3, "int main() { return 0 || 1 && 2 ? 3 + 5 * 12 : 5 / 3 * (1 + 2); }")
        )
    }
}

class Tests {

    private fun fileSource() = File("./src/test/resources")
        .listFiles()?.filter {
            it.name.endsWith(".hp")
        }

    @ParameterizedTest
    @ArgumentsSource(TestArgumentProvider::class)
    fun testParameters(expectedOutput: Int, inputString: String) {
        File("./build/test.hp").writeText(inputString)
        val output = compileAndRun("./build/test.hp")
        assertEquals(expectedOutput, output)
    }

    //    @TestFactory
    fun testFiles() = fileSource()?.map { file ->
        DynamicTest.dynamicTest(file.path) {
            compileAndRun(file.path)
        }
    }

    private fun compileAndRun(filename: String): Int {
        println("Program input:")
        println(File(filename).readText() + "\n")

        val tokens = Lexer().lex(filename)
        println("Lexical Analysis:")
        println(tokens.joinToString("\n") { it.prettyPrint() } + "\n")

        val ast = Ast().parseProgram(tokens)
        println("Parsed AST:")
        println("${ast.prettyPrint()}\n")
        println("${ast}\n")

        val assembly = Generator().generateProgram(ast)
        println("Generated Assembly:")
        println("$assembly\n")

        val fileNameWithoutExtension = filename.removeSuffix(".hp")
        val assemblyFileName = "$fileNameWithoutExtension.s"
        val executableFileName = "$fileNameWithoutExtension.exe"
        println(executableFileName)

        File(assemblyFileName).takeIf { it.exists() }?.delete()
        File(assemblyFileName).writeText(assembly)

        File(executableFileName).takeIf { it.exists() }?.delete()
        val processBuilder = ProcessBuilder("gcc", assemblyFileName, "-o", fileNameWithoutExtension)
        processBuilder.redirectErrorStream(true)
        val process1 = processBuilder.start()
        val bufferedReader = BufferedReader(InputStreamReader(process1.inputStream))
        var line = bufferedReader.readLine()
        while (line != null) {
            println(line)
            line = bufferedReader.readLine()
        }
        println("GCC: " + process1.waitFor())
        val process2 = Runtime.getRuntime().exec("\"$executableFileName\"")
        val status = process2.waitFor()
        println("Output is: $status")
        return status
    }
}