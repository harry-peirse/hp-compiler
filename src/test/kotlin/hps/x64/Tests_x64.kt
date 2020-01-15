package com.aal.hp.x64.com.aal.hp.x64

import hps.Ast
import hps.Lexer
import hps.atomicInt
import hps.x64.Generator
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
import java.util.concurrent.atomic.AtomicInteger
import java.util.stream.Stream

class TestArgumentProvider : ArgumentsProvider {
    override fun provideArguments(context: ExtensionContext?): Stream<out Arguments> {
        return Stream.of(
//            Arguments.of(2, "main :: (): s64 { var a: f64 = 0.5 * 4.0; var b: s64 = (s64)a; return b; }"),
            Arguments.of(2, "main :: s64 = 2"),
            Arguments.of(
                0, """
                |main :: (): s64 {
                |   putchar(72);
                |   putchar(101);
                |   putchar(108);
                |   putchar(108);
                |   putchar(111);
                |   putchar(44);
                |   putchar(32);
                |   putchar(87);
                |   putchar(111);
                |   putchar(114);
                |   putchar(108);
                |   putchar(100);
                |   putchar(33);
                |   putchar(10);
                |   return 0;
                |}""".trimMargin()
            ),
            Arguments.of(
                55, """
                |fib :: (n: s64): s64 {
                |    if (n == 0 || n == 1) {
                |        return n;
                |    } else {
                |        return fib(n - 1) + fib(n - 2);
                |    }
                |}
                |
                |main::(): s64 {
                |    var n: s64 = 10;
                |    return fib(n);
                |}""".trimMargin()
            ),
            Arguments.of(1, "foo::s64=1 bar::s64=foo()main::s64=bar()"),
            Arguments.of(1, "bar::(a: s64): s64{return a;}main :: (): s64 { return bar(1); }"),
            Arguments.of(0, "one::(): s64 { return 1; } \nmain :: (): s64 { return 1 - one(); }"),
            Arguments.of(2, "plusOne::(i: s64): s64 { return i + 1; } \nmain :: (): s64 { return plusOne(1); }"),
            Arguments.of(3, "add:: (a: s64, b: s64): s64 { return a + b; } \nmain :: (): s64 { return add(1, 2); }"),
            Arguments.of(8, "main :: (): s64 { var a: s64 = 2; for var i: s64=0; i<2; i = i + 1 a = a *2; return a; }"),
            Arguments.of(8, "main :: s64 { var a: s64; for(a = 2; a<7; a = a+2) ; return a; }"),
            Arguments.of(
                2,
                "main :: s64 { var a: s64 = 2; var b: s64 = 0; while a>0 {b = b + 1; a = a - 1;} return b; }"
            ),
            Arguments.of(
                2,
                "main :: (): s64 { var a: s64 = 2; var b: s64 = 0; do {b = b + 1; a = a - 1;} while a > 0 return b }"
            ),
            Arguments.of(
                6,
                "main :: (): s64 { var b: s64 = 0; for(var i: s64 = 0; i < 5; i = i + 1) { b = b + i; if(i>=3) break; } return b; }"
            ),
            Arguments.of(
                4,
                "main :: (): s64 { var a: s64 = 0; for(var i: s64 = 0; i < 3; i = i + 1) { if(i / 2 == 1) continue; a = a + 2; } return a; }"
            ),
            Arguments.of(3, "main :: (): s64 { return 1 + 2; }"),
            Arguments.of(7, "main :: (): s64 { return 1 + 2 * 3; }"),
            Arguments.of(13, "main :: (): s64 { return 2 * 2 + 3 * 3; }"),
            Arguments.of(30, "main :: (): s64 { return 2 * (2 + 3) * 3 }"),
            Arguments.of(43, "main :: (): s64 { return 5 / 4 + 6 * 7 }"),
            Arguments.of(38, "main :: (): s64 { return -3 - 5 / 4 + 6 * 7 }"),
            Arguments.of(35, "main :: (): s64 { return 2 * -3 - 5 / 4 + 6 * 7 }"),
            Arguments.of(47, "main :: (): s64 { return 2 * 3 - 5 / 4 + 6 * 7 }"),
            Arguments.of(36, "main :: (): s64 { return 1 + 2 * -3 - 5 / 4 + 6 * 7 }"),
            Arguments.of(1, "main :: (): s64 { return 1 and 1 }"),
            Arguments.of(0, "main :: (): s64 { return 1 == 2 }"),
            Arguments.of(1, "main :: (): s64 { return 0 || 1 }"),
            Arguments.of(1, "main :: s64 = 1 or 1"),
            Arguments.of(0, "main :: s64 { return 0 || 0 }"),
            Arguments.of(3, "main :: s64 { var a: s64 = 1; return a + 2; }"),
            Arguments.of(3, "main :: s64 { var a: s64 = 1; var b: s64 = 2; return a + b; }"),
            Arguments.of(2, "main :: s64 { var a: s64 = 1; a = 2; return a; }"),
            Arguments.of(4, "main :: s64 { var a: s64; a = 4; return a; }"),
            Arguments.of(1, "main :: s64 { var a: s64 = 2; if(a > 1) a = 1; return a; }"),
            Arguments.of(2, "main :: (): s64 { var a: s64; if(0) a = 1; else a = 2; return a; }"),
            Arguments.of(3, "main :: (): s64 { return 0 || 1 ? 3 : 5; }"),
            Arguments.of(63, "main :: (): s64 { return 0 || 1 && 2 ? 3 + 5 * 12 : 5 / 3 * (1 + 2); }"),
            Arguments.of(3, "main :: (): s64 { var a: s64 = 2; if(a > 1) { a = 1; a = a + 2; } return a; }"),
            Arguments.of(
                7,
                "main :: (): s64 { var a: s64 = 2; var b: s64 = 4; {a = 3; var b: s64 = 10;} return a + b; }"
            ),
            Arguments.of(1, "main :: (): s64 { var a: s64 = 1; {var a: s64 = 2;} return a; }"),
            Arguments.of(1, "main :: (): s64 { var a: s64 = 1; var b: s64 = 2; return a; }")
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

        val ast = Ast().parseProgram(tokens)
        println("Parsed AST:")
        println("${ast.prettyPrint()}\n")
        println("${ast}\n")

        val assembly = Generator().generateProgram(ast)
        println("Generated Assembly:")
        var lineNumber = 1
        println(assembly.split("\n").joinToString("\n") { "${lineNumber++}  $it" } + "\n")

        val fileNameWithoutExtension =
            outputPath.toString() + '\\' + filename.removeSuffix(".hp").split("\\").last().split("/").last()
        val assemblyFileName = "$fileNameWithoutExtension.s"
        val executableFileName = "$fileNameWithoutExtension.exe"
        println(executableFileName)

        File(assemblyFileName).takeIf { it.exists() }?.delete()
        File(assemblyFileName).writeText(assembly)

        File(executableFileName).takeIf { it.exists() }?.delete()
        val processBuilder1 = ProcessBuilder("gcc", "-m64", "-g", assemblyFileName, "-o", fileNameWithoutExtension)
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