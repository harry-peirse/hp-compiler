package com.aal.hp.c

import com.aal.hp.Code
import com.aal.hp.Code.*

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

            return """${toCType(function.returnType)} ${function.name}(${function.arguments.joinToString(", ") {
                "${toCType(
                    it.type
                )} ${it.name}"
            }}) {
                |$content
                |}""".trimMargin()
        }

        private inner class BlockItemGenerator(
            val variableMap: MutableMap<String, Int> = mutableMapOf(),
            val localScope: MutableSet<String> = mutableSetOf(),
            val parentCount: Int = 0,
            var stackIndex: Int = -8,
            var continueLabel: String? = null,
            var breakLabel: String? = null
        ) {
            fun generateBlockItem(statement: BlockItem): String = when (statement) {
                is BlockItem.Statement.Return -> "return ${generateExpression(statement.expression)};".trimMargin()
                is BlockItem.Statement.ProxyExpression -> "${generateExpression(statement.expression)};"
                is BlockItem.Statement.Conditional ->
                    if (statement.elseStatement != null) {
                        "if (${generateExpression(statement.condition)}) ${generateBlockItem(statement.statement)} else ${generateBlockItem(
                            statement.elseStatement
                        )}"
                    } else {
                        "if (${generateExpression(statement.condition)}) ${generateBlockItem(statement.statement)}"
                    }
                is BlockItem.Statement.Compound -> {
                    val blockItemGenerator =
                        BlockItemGenerator(
                            variableMap.toMutableMap(),
                            mutableSetOf(),
                            localScope.size,
                            stackIndex,
                            continueLabel,
                            breakLabel
                        )
                    """{
                        |  ${statement.blockItems.map { blockItemGenerator.generateBlockItem(it) }.joinToString("\n")}
                        |}""".trimMargin()
                }
                is BlockItem.Statement.ForDeclaration ->
                    "for (${generateBlockItem(statement.declaration)} ${generateExpression(statement.condition)}; ${generateExpression(
                        statement.increment
                    )}) ${generateBlockItem(statement.statement)}"
                is BlockItem.Statement.For ->
                    "for (${generateExpression(statement.initialization)}; ${generateExpression(statement.condition)};  ${generateExpression(
                        statement.increment
                    )}) ${generateBlockItem(statement.statement)}"
                is BlockItem.Statement.While -> "while (${generateExpression(statement.condition)}) ${generateBlockItem(
                    statement.statement
                )}"
                is BlockItem.Statement.DoWhile -> "do ${generateBlockItem(statement.statement)} while (${generateExpression(
                    statement.condition
                )});"
                is BlockItem.Statement.Continue -> "continue;"
                is BlockItem.Statement.Break -> "break;"
                is BlockItem.Declaration ->
                    if (localScope.contains(statement.variableName)) throw IllegalStateException("Variable ${statement.variableName} is already declared!")
                    else {
                        variableMap[statement.variableName] = stackIndex
                        localScope.add(statement.variableName)
                        stackIndex -= 8
                        "${toCType(statement.type)} ${statement.variableName}${if (statement.expression != null) " = ${generateExpression(
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
                is Expression.Unary -> "(${toCOperator(expression.unaryOp.value)} ${generateExpression(expression.expression)}) "
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