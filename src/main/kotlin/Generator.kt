package com.aal.hp

object Generator {

    fun generateProgram(program: Ast.Program) = generateFunction(program.functionDeclaration)

    private fun generateFunction(function: Ast.Function) = """|  .globl _${function.name}
                |_${function.name}:
                |${function.statements.map { generateStatement(it) }.joinToString() }
                """.trimMargin()

    private fun generateStatement(statement: Ast.Statement) = when (statement) {
        is Ast.Statement.Return -> """${generateExpression(statement.expression)}
                |  ret
                """.trimMargin()
        is Ast.Statement.ProxyExpression -> generateExpression(statement.expression)
        is Ast.Statement.Declare -> "" //TODO
    }

    private fun generateExpression(expression: Ast.Expression): String = when (expression) {
        is Ast.Expression.Constant -> """  movl  $${expression.value},   %eax""".trimMargin()
        is Ast.Expression.Assign -> "" //TODO
        is Ast.Expression.Variable -> "" //TODO
        is Ast.Expression.Nested -> generateExpression(expression.expression)
        is Ast.Expression.Unary -> when (expression.unaryOp.type) {
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
        is Ast.Expression.Binary -> when (expression.binaryOp.type) {
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