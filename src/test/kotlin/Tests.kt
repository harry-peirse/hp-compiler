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
            Arguments.of(
                0, """
                |int main() {
                |    putchar(72);
                |    putchar(101);
                |    putchar(108);
                |    putchar(108);
                |    putchar(111);
                |    putchar(44);
                |    putchar(32);
                |    putchar(87);
                |    putchar(111);
                |    putchar(114);
                |    putchar(108);
                |    putchar(100);
                |    putchar(33);
                |    putchar(10);
                |    return 0;
                |}""".trimMargin()
            ),
            Arguments.of(
                55, """
                |int fib(int n) {
                |    if (n == 0 || n == 1) {
                |        return n;
                |    } else {
                |        return fib(n - 1) + fib(n - 2);
                |    }
                |}
                |
                |int main() {
                |    int n = 10;
                |    return fib(n);
                |}""".trimMargin()
            ),
            Arguments.of(0, "int one() { return 1; } \nint main() { return 1 - one(); }"),
            Arguments.of(2, "int plusOne(int i) { return i + 1; } \nint main() { return plusOne(1); }"),
            Arguments.of(3, "int add(int a, int b) { return a + b; } \nint main() { return add(1, 2); }"),
            Arguments.of(8, "int main() { int a = 2; for(int i=0; i<2; i = i + 1) a = a *2; return a; }"),
            Arguments.of(8, "int main() { int a; for(a = 2; a<7; a = a+2) ; return a; }"),
            Arguments.of(2, "int main() { int a = 2; int b = 0; while(a>0) {b = b + 1; a = a - 1;} return b; }"),
            Arguments.of(2, "int main() { int a = 2; int b = 0; do {b = b + 1; a = a - 1;} while(a>0)  return b; }"),
            Arguments.of(
                6,
                "int main() { int b = 0; for(int i = 0; i < 5; i = i + 1) { b = b + i; if(i>=3) break; } return b; }"
            ),
            Arguments.of(
                4,
                "int main() { int a = 0; for(int i = 0; i < 3; i = i + 1) { if(i / 2 == 1) continue; a = a + 2; } return a; }"
            ),
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
            Arguments.of(2, "int main() { int a; if(0) a = 1; else a = 2; return a; }"),
            Arguments.of(3, "int main() { return 0 || 1 ? 3 : 5; }"),
            Arguments.of(63, "int main() { return 0 || 1 && 2 ? 3 + 5 * 12 : 5 / 3 * (1 + 2); }"),
            Arguments.of(3, "int main() { int a = 2; if(a > 1) { a = 1; a = a + 2; } return a; }"),
            Arguments.of(7, "int main() { int a = 2; int b = 4; {a = 3; int b = 10;} return a + b; }"),
            Arguments.of(1, "int main() { int a = 1; {int a = 2;} return a; }"),
            Arguments.of(1, "int main() { int a = 1; int b = 2; return a; }")
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
        val processBuilder1 = ProcessBuilder("gcc", assemblyFileName, "-o", fileNameWithoutExtension)
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