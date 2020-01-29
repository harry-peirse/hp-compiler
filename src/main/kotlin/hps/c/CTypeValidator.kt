package hps.c

class Validator {

    fun validate(program: CCode.Program): String {
        val functionsByName = program.functions.associateBy { it.name.value }
        val functionsToForwardDeclare = mutableSetOf<CCode.Function.Implementation>()

        val structsByName = program.structs.associateBy { it.name.value }
//        val structsToForwardDeclare = mutableSetOf<CCode.Struct>()

        val imports = program.functions
            .filterIsInstance<CCode.Function.External>()
            .mapNotNull { when(it.name.value) {
                "putchar" -> "stdio"
                else -> null
            } }
            .toSet()
            .joinToString("\n") { "#include <$it.h>" }

        program.functions
            .filterIsInstance<CCode.Function.Implementation>()
            .flatMap { it.blockItems }
            .flatMap { it.expressions() + it.blockItems().flatMap { bi -> bi.expressions() } }
            .flatMap { it.flattened() }
            .filterIsInstance<CCode.Expression.FunctionCall>()
            .forEach {
                if (!structsByName.containsKey(it.name.value)) {
                    if (!functionsByName.containsKey(it.name.value)) {
                        error("Unknown Function call: ${it.name}")
                    }
                    if ((it.name.pos < functionsByName[it.name.value]?.type?.pos ?: error("ERROR")) && functionsByName[it.name.value]
                            ?: error("ERROR") is CCode.Function.Implementation
                    ) functionsToForwardDeclare.add(functionsByName[it.name.value] as CCode.Function.Implementation)
                } else {
                    it.isConstructor = true
                }
            }

        program.structs
            .flatMap { it.arguments }
            .filterNot {
                listOf("char", "int", "float", "char[]", "int[]", "char[]").contains(it.type.value)
            }
            .forEach {
                if (!structsByName.containsKey(it.type.value)) error("Unknown Struct: ${it.type}")
                // TODO: Forward declare
            }

        return imports + "\n\n" + functionsToForwardDeclare.joinToString("") {
            "${it.type.value} ${it.name.value}(${it.arguments.joinToString(
                ", "
            ) { arg -> "${arg.type.value} ${arg.name.value}" }});\n\n"
        } + program.toC()
    }
}