package com.aal.hp

class Generator {

    fun generateProgram(program: Program) = FunctionGenerator().generateFunction(program.functions[0]) + "\n"

    private inner class FunctionGenerator {
        fun generateFunction(function: Function): String {
            val blockItemGenerator = BlockItemGenerator()
            return """|  .globl _${function.name}
            |_${function.name}:
            |  push  %ebp
            |  movl  %esp, %ebp
            |${function.blockItems.map {
                blockItemGenerator.generateBlockItem(it)
            }.joinToString(
                "\n"
            )}
            """.trimMargin()
        }

        private inner class BlockItemGenerator(
            val variableMap: MutableMap<String, Int> = mutableMapOf(),
            var stackIndex: Int = -4,
            var continueLabel: String? = null,
            var breakLabel: String? = null
        ) {
            val localScope: MutableSet<String> = mutableSetOf()
            fun generateBlockItem(statement: BlockItem): String = when (statement) {
                is BlockItem.Statement.Return -> """${generateExpression(statement.expression)}
                    |  movl  %ebp, %esp
                    |  pop   %ebp
                    |  ret""".trimMargin()
                is BlockItem.Statement.ProxyExpression -> generateExpression(statement.expression)
                is BlockItem.Statement.Conditional ->
                    if (statement.elseStatement != null) {
                        val label1 = generateLabel("if_")
                        val label2 = generateLabel("else_")
                        """${generateExpression(statement.condition)}
                            |  cmpl  $0,   %eax
                            |  je    $label1
                            |${generateBlockItem(statement.statement)}
                            |  jmp   $label2
                            |$label1:
                            |${generateBlockItem(statement.elseStatement)}
                            |$label2:""".trimMargin()
                    } else {
                        val label1 = generateLabel("if_")
                        """${generateExpression(statement.condition)}
                            |  cmpl  $0,   %eax
                            |  je    $label1
                            |${generateBlockItem(statement.statement)}
                            |$label1:""".trimMargin()
                    }
                is BlockItem.Statement.Compound -> {
                    val blockItemGenerator =
                        BlockItemGenerator(variableMap.toMutableMap(), stackIndex, continueLabel, breakLabel)
                    """${statement.blockItems.map {
                        blockItemGenerator.generateBlockItem(it)
                    }.joinToString("\n")}
                        |  addl  $${blockItemGenerator.localScope.size * 4},   %esp""".trimMargin()
                }
                is BlockItem.Statement.ForDeclaration -> {
                    continueLabel = generateLabel("for1_")
                    breakLabel = generateLabel("for2_")
                    val label = generateLabel("for3_")
                    """${generateBlockItem(statement.declaration)}
                        |$label:
                        |${generateExpression(statement.condition)}
                        |  cmpl  $0,   %eax
                        |  je    $breakLabel
                        |${generateBlockItem(statement.statement)}
                        |$continueLabel:
                        |${generateExpression(statement.increment)}
                        |  jmp   $label
                        |$breakLabel:""".trimMargin()
                }
                is BlockItem.Statement.For -> {
                    continueLabel = generateLabel("for1_")
                    breakLabel = generateLabel("for2_")
                    val label = generateLabel("for3_")
                    """${generateExpression(statement.initialization)}
                        |$label:
                        |${generateExpression(statement.condition)}
                        |  cmpl  $0,   %eax
                        |  je    $breakLabel
                        |${generateBlockItem(statement.statement)}
                        |$continueLabel:
                        |${generateExpression(statement.increment)}
                        |  jmp   $label
                        |$breakLabel:""".trimMargin()
                }
                is BlockItem.Statement.While -> {
                    continueLabel = generateLabel("while1_")
                    breakLabel = generateLabel("while2_")
                    """$continueLabel:
                        |${generateExpression(statement.condition)}
                        |  cmpl  $0,   %eax
                        |  je    $breakLabel
                        |${generateBlockItem(statement.statement)}
                        |  jmp   $continueLabel
                        |$breakLabel:""".trimMargin()
                }
                is BlockItem.Statement.DoWhile -> {
                    continueLabel = generateLabel("do1_")
                    breakLabel = generateLabel("do2_")
                    """$continueLabel:
                        |${generateBlockItem(statement.statement)}
                        |${generateExpression(statement.condition)}
                        |  cmpl  $0,   %eax
                        |  jne   $continueLabel
                        |$breakLabel:""".trimMargin()
                }
                is BlockItem.Statement.Continue -> if (continueLabel != null) "  jmp   $continueLabel" else throw IllegalStateException()
                is BlockItem.Statement.Break -> if (breakLabel != null) "  jmp   $breakLabel" else throw IllegalStateException()
                is BlockItem.Declaration ->
                    if (localScope.contains(statement.variableName)) throw IllegalStateException("Variable ${statement.variableName} is already declared!")
                    else {
                        variableMap[statement.variableName] = stackIndex
                        localScope.add(statement.variableName)
                        stackIndex -= 4
                        "${if (statement.expression != null) generateExpression(statement.expression) + "\n" else ""}  pushl %eax"
                    }
            }

            private fun generateExpression(expression: Expression): String = when (expression) {
                is Expression.Constant -> """|  movl  $${expression.value},   %eax""".trimMargin()
                is Expression.Assign -> """${generateExpression(expression.expression)}
                    |  movl  %eax, ${variableMap[expression.variableName]}(%ebp)""".trimMargin()
                is Expression.Variable -> """|  movl  ${variableMap[expression.variableName]}(%ebp), %eax""".trimMargin()
                is Expression.Nested -> generateExpression(expression.expression)
                is Expression.Conditional -> {
                    val label1 = generateLabel("if_")
                    val label2 = generateLabel("else_")
                    """${generateExpression(expression.condition)}
                        |  cmpl  $0,   %eax
                        |  je    $label1
                        |${generateExpression(expression.expression)}
                        |  jmp   $label2
                        |$label1:
                        |${generateExpression(expression.elseExpression)}
                        |$label2:""".trimMargin()
                }
                is Expression.Empty -> ""
                is Expression.FunctionCall -> "" // TODO
                is Expression.Unary -> when (expression.unaryOp.type) {
                    Symbol.MINUS -> """${generateExpression(expression.expression)}
                        |  neg   %eax""".trimMargin()
                    Symbol.TILDA -> """${generateExpression(expression.expression)}
                        |  not   %eax""".trimMargin()
                    Symbol.BANG -> """${generateExpression(expression.expression)}
                        |  cmpl  $0,   %eax
                        |  movl  $0,   %eax
                        |  sete  %al""".trimMargin()
                    else -> throw IllegalStateException()
                }
                is Expression.Binary -> when (expression.binaryOp.type) {
                    Symbol.PLUS -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  addl  %ecx, %eax""".trimMargin()
                    Symbol.MINUS -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  subl  %eax, %ecx
                        |  movl  %ecx, %eax""".trimMargin()
                    Symbol.ASTERISK -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  imul  %ecx, %eax""".trimMargin()
                    Symbol.SLASH -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  movl  %eax, %ecx
                        |  pop   %eax
                        |  cdq
                        |  idivl %ecx""".trimMargin()
                    Symbol.EQUAL -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  sete  %al""".trimMargin()
                    Symbol.NOT_EQUAL -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  setne %al""".trimMargin()
                    Symbol.LESS_THAN -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  setl  %al""".trimMargin()
                    Symbol.LESS_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  setle %al""".trimMargin()
                    Symbol.GREATER_THAN -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  setg  %al""".trimMargin()
                    Symbol.GREATER_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                        |  push  %eax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %ecx
                        |  cmpl  %eax, %ecx
                        |  movl  $0,   %eax
                        |  setge %al""".trimMargin()
                    Symbol.OR -> {
                        val label1 = generateLabel("or1_")
                        val label2 = generateLabel("or2_")
                        """${generateExpression(expression.firstExpression)}
                            |  cmpl  $0,   %eax
                            |  je    $label1
                            |  movl  $1,   %eax
                            |  jmp   $label2
                            |$label1:
                            |${generateExpression(expression.secondExpression)}
                            |  cmpl  $0,   %eax
                            |  movl  $0,   %eax
                            |  setne %al
                            |$label2:""".trimMargin()
                    }
                    Symbol.AND -> {
                        val label1 = generateLabel("and1_")
                        val label2 = generateLabel("and2_")
                        """${generateExpression(expression.firstExpression)}
                            |  cmpl  $0,   %eax
                            |  jne   $label1
                            |  jmp   $label2
                            |$label1:
                            |${generateExpression(expression.secondExpression)}
                            |  cmpl  $0,   %eax
                            |  movl  $0,   %eax
                            |  setne %al
                            |$label2:""".trimMargin()
                    }
                    else -> throw IllegalStateException()
                }
            }
        }
    }

    private var int: Int = 0
    fun generateLabel(prefix: String) = "_$prefix${int++}"
}