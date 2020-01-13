package com.aal.hp

class Generator {

    fun generateProgram(program: Program) = generateFunction(program.functionDeclaration) + "\n"

    private val variableMap = mutableMapOf<String, Int>()
    private var stackIndex = -4

    private fun generateFunction(function: Function) = """|  .globl _${function.name}
                |_${function.name}:
                |  push  %ebp
                |  movl  %esp, %ebp
                |${function.blockItems.map { generateBlockItem(it) }.joinToString("\n")}
                """.trimMargin()

    private fun generateBlockItem(statement: BlockItem) = when (statement) {
        is BlockItem.Statement.Return -> """${generateExpression(statement.expression)}
                |  movl  %ebp, %esp
                |  pop   %ebp
                |  ret
                """.trimMargin()
        is BlockItem.Statement.ProxyExpression -> generateExpression(statement.expression)
        is BlockItem.Statement.Conditional -> "" // TODO
        is BlockItem.Declaration ->
            if (variableMap.containsKey(statement.variableName)) throw IllegalStateException("Variable ${statement.variableName} is already declared!")
            else {
                variableMap[statement.variableName] = stackIndex
                stackIndex -= 4
                "${if (statement.expression != null) generateExpression(statement.expression) + "\n" else ""}  pushl %eax"
            }
    }

    private fun generateExpression(expression: Expression): String = when (expression) {
        is Expression.Constant -> """  movl  $${expression.value},   %eax""".trimMargin()
        is Expression.Assign -> """${generateExpression(expression.expression)}
            |  movl  %eax, ${variableMap[expression.variableName]}(%ebp)""".trimMargin()
        is Expression.Variable -> """  movl  ${variableMap[expression.variableName]}(%ebp), %eax""".trimMargin()
        is Expression.Nested -> generateExpression(expression.expression)
        is Expression.Conditional -> "" // TODO
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
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  sete  %al""".trimMargin()
            Symbol.NOT_EQUAL -> """${generateExpression(expression.firstExpression)}
                |  push  %eax
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  setne %al""".trimMargin()
            Symbol.LESS_THAN -> """${generateExpression(expression.firstExpression)}
                |  push  %eax
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  setl  %al""".trimMargin()
            Symbol.LESS_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                |  push  %eax
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  setle %al""".trimMargin()
            Symbol.GREATER_THAN -> """${generateExpression(expression.firstExpression)}
                |  push  %eax
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  setg  %al""".trimMargin()
            Symbol.GREATER_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                |  push  %eax
                |  ${generateExpression(expression.secondExpression)}
                |  pop   %ecx
                |  cmpl  %eax, %ecx
                |  movl  $0,   %eax
                |  setge %al""".trimMargin()
            Symbol.OR -> {
                val label1 = generateLabel()
                val label2 = generateLabel()
                """${generateExpression(expression.firstExpression)}
                |  cmpl  $0,   %eax
                |  je    $label1
                |  movl  $1,   %eax
                |  jmp   $label2
                |$label1:
                |  ${generateExpression(expression.secondExpression)}
                |  cmpl  $0,   %eax
                |  movl  $0,   %eax
                |  setne %al
                |$label2:""".trimMargin()
            }
            Symbol.AND -> {
                val label1 = generateLabel()
                val label2 = generateLabel()
                """${generateExpression(expression.firstExpression)}
                |  cmpl  $0,   %eax
                |  jne   $label1
                |  jmp   $label2
                |$label1:
                |  ${generateExpression(expression.secondExpression)}
                |  cmpl  $0,   %eax
                |  movl  $0,   %eax
                |  setne %al
                |$label2:""".trimMargin()
            }
            else -> throw IllegalStateException()
        }
    }

    private var int: Int = 0
    private fun generateLabel() = "_label_${int++}"
}