package hps.c

import hps.Code
import hps.Code.*

class Generator {

    fun toCType(type: String) = when (type) {
        "s64" -> "int"
        "f64" -> "float"
        else -> type
    }

    fun toCOperator(op: String) = when (op) {
        "and" -> "&&"
        "or" -> "||"
        else -> op
    }

    fun generateProgram(program: Program) = program.functions.joinToString("\n") {
        "${toCType(it.returnType)} ${it.name}(${it.arguments.joinToString(", ") {
            toCType(it.type)
        }});"
    } + "\n" + program.functions.joinToString("\n") { FunctionGenerator().generateFunction(it) } + "\n"

    private inner class FunctionGenerator {

        val variableMap = mutableMapOf<String, Int>()
        val localScope = mutableSetOf<String>()
        var argumentOffset: Int = 8

        fun generateFunction(function: Code.Function): String {
            function.arguments.forEach {
                variableMap[it.name] = argumentOffset
                localScope.add(it.name)
                argumentOffset += 8
            }
            val blockItemGenerator = BlockItemGenerator(variableMap, localScope)
            val content = function.blockItems.joinToString("\n") {
                blockItemGenerator.generateBlockItem(it)
            }

            return "${toCType(function.returnType)} ${function.name}(${function.arguments.joinToString(", ") {
                "${toCType(it.type)} ${it.name}"
            }})\n{\n$content\n}"
        }

        private inner class BlockItemGenerator(
            val variableMap: MutableMap<String, Int> = mutableMapOf(),
            val localScope: MutableSet<String> = mutableSetOf(),
            val depth: Int = 1,
            var stackIndex: Int = -8,
            var continueLabel: String? = null,
            var breakLabel: String? = null
        ) {

            val padding = "    ".repeat(depth)

            fun generateBlockItem(statement: BlockItem, padding: String = this.padding): String = when (statement) {
                is BlockItem.Statement.Return -> "${padding}return ${generateExpression(statement.expression)};"
                is BlockItem.Statement.ProxyExpression -> "${padding}${generateExpression(statement.expression)};"
                is BlockItem.Statement.Conditional ->
                    if (statement.elseStatement != null) {
                        "${padding}if (${generateExpression(statement.condition)}) ${generateBlockItem(
                            statement.statement,
                            ""
                        )}\n${padding}else ${generateBlockItem(
                            statement.elseStatement, ""
                        )}"
                    } else {
                        "${padding}if (${generateExpression(statement.condition)}) ${generateBlockItem(
                            statement.statement,
                            ""
                        )}"
                    }
                is BlockItem.Statement.Compound -> {
                    val blockItemGenerator =
                        BlockItemGenerator(
                            variableMap.toMutableMap(),
                            mutableSetOf(),
                            depth + 1,
                            stackIndex,
                            continueLabel,
                            breakLabel
                        )
                    "$padding{\n${statement.blockItems.map { blockItemGenerator.generateBlockItem(it) }.joinToString(
                        "\n"
                    )}\n$padding}"
                }
                is BlockItem.Statement.ForDeclaration ->
                    "${padding}for (${generateBlockItem(statement.declaration).trim()} ${generateExpression(statement.condition)}; ${generateExpression(
                        statement.increment
                    )})\n${generateBlockItem(statement.statement)}"
                is BlockItem.Statement.For ->
                    "${padding}for (${generateExpression(statement.initialization)}; ${generateExpression(statement.condition)};  ${generateExpression(
                        statement.increment
                    )})\n${generateBlockItem(statement.statement)}"
                is BlockItem.Statement.While -> "${padding}while (${generateExpression(statement.condition)})\n${generateBlockItem(
                    statement.statement
                )}"
                is BlockItem.Statement.DoWhile -> "${padding}do ${generateBlockItem(statement.statement)} while (${generateExpression(
                    statement.condition
                )});"
                is BlockItem.Statement.Continue -> "${padding}continue;"
                is BlockItem.Statement.Break -> "${padding}break;"
                is BlockItem.Declaration ->
                    if (localScope.contains(statement.variableName)) throw IllegalStateException("Variable ${statement.variableName} is already declared!")
                    else {
                        variableMap[statement.variableName] = stackIndex
                        localScope.add(statement.variableName)
                        stackIndex -= 8
                        "${padding}${toCType(statement.type)} ${statement.variableName}${if (statement.expression != null) " = ${generateExpression(
                            statement.expression
                        )};" else ";"}"
                    }
            }

            private fun generateExpression(expression: Expression): String = when (expression) {
                is Expression.Constant -> expression.value
                is Expression.Assign -> "${expression.variableName} = ${generateExpression(expression.expression)}"
                is Expression.Variable -> expression.variableName
                is Expression.Nested -> "(${generateExpression(expression.expression)})"
                is Expression.Conditional -> "${generateExpression(expression.condition)} ? ${generateExpression(
                    expression.expression
                )} : ${generateExpression(expression.elseExpression)}"
                is Expression.Empty -> ""
                is Expression.FunctionCall -> "${expression.name}( ${expression.arguments.joinToString(" , ") {
                    generateExpression(it)
                }} )"
                is Expression.Unary ->
                    if (expression.postfix) "(${generateExpression(expression.expression)}${toCOperator(expression.unaryOp.value)}) "
                    else "(${toCOperator(expression.unaryOp.value)}${generateExpression(expression.expression)}) "
                is Expression.Cast -> "" // TODO
                is Expression.Binary -> "(${generateExpression(expression.firstExpression)} ${toCOperator(expression.binaryOp.value)} ${generateExpression(
                    expression.secondExpression
                )})"
            }
        }
    }

    private var int: Int = 0
    fun generateLabel(prefix: String) = "_$prefix${int++}"
}