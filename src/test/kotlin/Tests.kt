package com.aal.hp

import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import java.io.File

class Tests {

    private fun fileSource() = File("./src/test/resources")
        .listFiles()?.filter {
            it.name.endsWith(".hp")
        }

    @TestFactory
    fun test() = fileSource()?.map { file ->
        DynamicTest.dynamicTest(file.path) {
            println("Program input:")
            println(File(file.path).readText() + "\n")

            val tokens = Lexer.lex(file.path)
            println("Lexical Analysis:")
            println(tokens.joinToString("\n") + "\n")

            val ast = Ast.parseProgram(tokens)
            println("Parsed AST:")
            println("$ast\n")

            val assembly = Generator.generateProgram(ast)
            println("Generated Assembly:")
            println("$assembly\n")

            val fileNameWithoutExtension = file.path.removeSuffix(".hp")
            val assemblyFileName = "$fileNameWithoutExtension.s"
            val executableFileName = "$fileNameWithoutExtension.exe"
            println(executableFileName)

            File(assemblyFileName).takeIf { it.exists() }?.delete()
            File(assemblyFileName).writeText(assembly)

            File(executableFileName).takeIf { it.exists() }?.delete()
            val process1 = Runtime.getRuntime().exec("gcc $assemblyFileName -o $fileNameWithoutExtension")
            println("GCC: " + process1.waitFor())
            val process2 = Runtime.getRuntime().exec("\"$executableFileName\"")
            println("Output is: " + process2.waitFor())
        }
    }
}