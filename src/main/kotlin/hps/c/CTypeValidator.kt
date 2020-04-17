package hps.c

import hps.Ast

data class FunctionSignature(val returnType: String, val name: String, val arguments: List<Pair<String, String>>)

class Validator {

    private val functionSignatures = mutableListOf<FunctionSignature>()

    fun validate(program: Ast.Program): String {
        program.functions.map { func ->
            FunctionSignature(
                func.type.value,
                func.name.value,
                func.arguments.map { arg -> arg.type.value to arg.name.value }.toList()
            )
        }.toCollection(functionSignatures)

        return functionSignatures.joinToString("\n") { func ->
            func.run {
                "${if (returnType == "s64") "int" else returnType} $name(${arguments.joinToString(", ") { args ->
                    args.run { "${if (first == "s64") "int" else first} $second" }
                }});"
            }
        } + "\n\n" + program.toC()
    }
}