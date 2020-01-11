package com.aal.hp

object Generator {

    fun generateProgram(program: Ast.Program) = generateFunction(program.functionDeclaration)

    private fun generateFunction(function: Ast.Function) = """|  .globl _${function.name}
                |_${function.name}:
                |${generateStatement(function.statement)}
                """.trimMargin()

    private fun generateStatement(statement: Ast.Statement) = when (statement) {
        is Ast.Statement.Return -> """${generateExpression(statement.expression)}
                |  ret
                """.trimMargin()
    }

    private fun generateExpression(expression: Ast.Expression): String = when (expression) {
        is Ast.Expression.Constant -> """  movl  $${expression.value},   %eax""".trimMargin()
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
        is Ast.Expression.Nested -> generateExpression(expression.expression)
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
            else -> throw IllegalStateException()
        }
    }
}