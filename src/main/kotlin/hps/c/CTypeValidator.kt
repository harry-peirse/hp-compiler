package hps.c

class Validator {

    fun validate(program: CCode.Program): String {
        val funcSigByName = program.functions.associateBy { it.name.value }
        val funcsToForwardDeclare = mutableSetOf<CCode.Function.Implementation>()

        program.functions
            .filterIsInstance<CCode.Function.Implementation>()
            .flatMap { it.blockItems }
            .flatMap { it.expressions() + it.blockItems().flatMap { bi -> bi.expressions() } }
            .flatMap { it.flattened() }
            .filterIsInstance<CCode.Expression.FunctionCall>()
            .forEach {
                if (!program.functions.map { func -> func.name.value }
                        .contains(it.name.value)) throw IllegalStateException("Unknown Function call: ${it.name}")
                if ((it.name.pos < funcSigByName[it.name.value]?.type?.pos ?: error("ERROR")) && funcSigByName[it.name.value]
                        ?: error("ERROR") is CCode.Function.Implementation
                ) {
                    funcsToForwardDeclare.add(funcSigByName[it.name.value] as CCode.Function.Implementation)
                }
            }

        return funcsToForwardDeclare.joinToString("") {
            "${it.type.value} ${it.name.value}(${it.arguments.joinToString(
                ", "
            ) { arg -> "${arg.type.value} ${arg.name.value}" }});\n\n"
        } + program.toC()
    }
}