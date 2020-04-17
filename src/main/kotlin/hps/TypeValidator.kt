package hps

import hps.HPCode.*

class Validator {

    fun validate(program: Program): String {
        val functionsByName = program.functions.associateBy { it.name.value }
        val functionsToForwardDeclare = mutableSetOf<Func.Implementation>()

        val structsByName = program.structs.associateBy { it.name.value }
        val structsToForwardDeclare = mutableSetOf<Struct>()

        val imports = program.functions
            .filterIsInstance<Func.External>()
            .mapNotNull { when(it.name.value) {
                "putchar" -> "stdio"
                else -> null
            } }
            .toSet()
            .joinToString("\n") { "#include <$it.h>" }

        program.functions
            .filterIsInstance<Func.Implementation>()
            .flatMap { it.blockItems }
            .flatMap { it.expressions() + it.blockItems().flatMap { bi -> bi.expressions() } }
            .flatMap { it.flattened() }
            .filterIsInstance<Expression.FunctionCall>()
            .forEach {
                if (!structsByName.containsKey(it.name.value)) {
                    if (!functionsByName.containsKey(it.name.value)) {
                        error("Unknown Function call: ${it.name}")
                    }
                    if ((it.name.pos < functionsByName[it.name.value]?.type?.pos ?: error("ERROR")) && functionsByName[it.name.value]
                            ?: error("ERROR") is Func.Implementation
                    ) functionsToForwardDeclare.add(functionsByName[it.name.value] as Func.Implementation)
                } else {
                    it.isConstructor = true
                }
            }

        program.structs
            .flatMap { it.arguments }
            .filterNot {
                listOf("char", "int", "float", "char[]", "int[]", "char[]", "void", "void[]").contains(it.type.value)
            }
            .forEach {
                if (!structsByName.containsKey(it.type.value)) error("Unknown Struct: ${it.type}")
                // TODO: Forward declare
            }

        return imports + "\n\n" + program.structs.joinToString(""){
            "typedef struct ${it.name.value}_ ${it.name.value};\n"
        } + functionsToForwardDeclare.joinToString("") {
            "${it.type.value}${if(it.isArray) "*" else ""} ${it.name.value}(${it.arguments.joinToString(
                ", "
            ) { arg -> "${arg.type.value} ${arg.name.value}" }});\n"
        } + "\n" + program.prettyPrint()
    }
}