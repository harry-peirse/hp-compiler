package hps.x64

import com.aal.hp.*
import hps.Code
import hps.Code.*
import hps.Symbol

class Generator {

    fun generateProgram(program: Program): String {
        return program.functions.joinToString("\n") { FunctionGenerator().generateFunction(it) } + "\n"
    }

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

            return """|  .globl ${function.name}
                |${function.name}:
                |  push  %r15  
                |  push  %r14
                |  push  %r13
                |  push  %rbp
                |  sub   $${32 + localScope.size * 8}, %rsp
                |  mov   %rsp, %rbp
                |${function.arguments.mapIndexed { index, _ ->
                when (index) {
                    0 -> "  mov   %rcx,  8(%rsp)\n"
                    1 -> "  mov   %rdx, 16(%rsp)\n"
                    2 -> "  mov   %r8,  24(%rsp)\n"
                    3 -> "  mov   %r9,  32(%rsp)\n"
                    else -> "  pop   %rcx\n" +
                            "  mov   %rcx, ${8 + 8 * index}(%rsp)\n"
                }
            }.joinToString("")}  lea   128(%rsp), %r13
                |$content""".trimMargin()
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
                is BlockItem.Statement.Return -> """${generateExpression(statement.expression)}
                    |  lea   -128(%r13), %rsp
                    |  mov   %rbp, %rsp
                    |  add   $${32 + (parentCount + localScope.size) * 8}, %rsp
                    |  pop   %rbp
                    |  pop   %r13
                    |  pop   %r14
                    |  pop   %r15
                    |  ret""".trimMargin()
                is BlockItem.Statement.ProxyExpression -> generateExpression(statement.expression)
                is BlockItem.Statement.Conditional ->
                    if (statement.elseStatement != null) {
                        val label1 = generateLabel("if_")
                        val label2 = generateLabel("else_")
                        """${generateExpression(statement.condition)}
                            |  cmp   $0,   %rax
                            |  je    $label1
                            |${generateBlockItem(statement.statement)}
                            |  jmp   $label2
                            |$label1:
                            |${generateBlockItem(statement.elseStatement)}
                            |$label2:""".trimMargin()
                    } else {
                        val label1 = generateLabel("if_")
                        """${generateExpression(statement.condition)}
                            |  cmp   $0,   %rax
                            |  je    $label1
                            |${generateBlockItem(statement.statement)}
                            |$label1:""".trimMargin()
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
                    """${statement.blockItems.map {
                        blockItemGenerator.generateBlockItem(it)
                    }.joinToString("\n")}
                        |  add   $${blockItemGenerator.localScope.size * 8},   %rsp""".trimMargin()
                }
                is BlockItem.Statement.ForDeclaration -> {
                    continueLabel = generateLabel("for1_")
                    breakLabel = generateLabel("for2_")
                    val label = generateLabel("for3_")
                    """${generateBlockItem(statement.declaration)}
                        |$label:
                        |${generateExpression(statement.condition)}
                        |  cmp   $0,   %rax
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
                        |  cmp   $0,   %rax
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
                        |  cmp   $0,   %rax
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
                        |  cmp   $0,   %rax
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
                        stackIndex -= 8
                        "${if (statement.expression != null) generateExpression(statement.expression) + "\n" else ""}  push  %rax"
                    }
            }

            private fun generateExpression(expression: Expression): String = when (expression) {
                is Expression.Constant -> "  mov   $${expression.value},   %rax"
                is Expression.Assign -> """${generateExpression(expression.expression)}
                    |  mov   %rax, ${variableMap[expression.variableName]}(%rbp)""".trimMargin()
                is Expression.Variable -> "  mov   ${variableMap[expression.variableName]}(%rbp), %rax"
                is Expression.Nested -> generateExpression(expression.expression)
                is Expression.Conditional -> {
                    val label1 = generateLabel("if_")
                    val label2 = generateLabel("else_")
                    """${generateExpression(expression.condition)}
                        |  cmp   $0,   %rax
                        |  je    $label1
                        |${generateExpression(expression.expression)}
                        |  jmp   $label2
                        |$label1:
                        |${generateExpression(expression.elseExpression)}
                        |$label2:""".trimMargin()
                }
                is Expression.Empty -> ""
                is Expression.FunctionCall -> """${expression.arguments.mapIndexed { index, it ->
                    when (index) {
                        0 -> "${generateExpression(it)}\n  mov   %rax, %rcx"
                        1 -> "${generateExpression(it)}\n  mov   %rax, %rdx"
                        2 -> "${generateExpression(it)}\n  mov   %rax, %r8"
                        3 -> "${generateExpression(it)}\n  mov   %rax, %r9"
                        else -> "${generateExpression(it)}\n  push  %rax"
                    }
                }.joinToString("\n")}
                    |  call  ${expression.name}
                    """.trimMargin() // |  add   $${expression.arguments.size * 8},   %rsp
                is Expression.Unary -> when (expression.unaryOp.type) {
                    Symbol.MINUS -> """${generateExpression(expression.expression)}
                        |  neg   %rax""".trimMargin()
                    Symbol.TILDA -> """${generateExpression(expression.expression)}
                        |  not   %rax""".trimMargin()
                    Symbol.BANG -> """${generateExpression(expression.expression)}
                        |  cmp   $0,   %rax
                        |  mov   $0,   %rax
                        |  sete  %al""".trimMargin()
                    else -> throw IllegalStateException()
                }
                is Expression.Cast -> "" // TODO
                is Expression.Binary -> when (expression.binaryOp.type) {
                    Symbol.PLUS -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  add   %rcx, %rax""".trimMargin()
                    Symbol.MINUS -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  sub   %rax, %rcx
                        |  mov   %rcx, %rax""".trimMargin()
                    Symbol.ASTERISK -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  imul  %rcx, %rax""".trimMargin()
                    Symbol.SLASH -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  mov   %rax, %rcx
                        |  pop   %rax
                        |  cdq
                        |  idiv  %rcx""".trimMargin()
                    Symbol.EQUAL -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  sete  %al""".trimMargin()
                    Symbol.NOT_EQUAL -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  setne %al""".trimMargin()
                    Symbol.LESS_THAN -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  setl  %al""".trimMargin()
                    Symbol.LESS_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  setle %al""".trimMargin()
                    Symbol.GREATER_THAN -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  setg  %al""".trimMargin()
                    Symbol.GREATER_THAN_OR_EQUAL_TO -> """${generateExpression(expression.firstExpression)}
                        |  push  %rax
                        |${generateExpression(expression.secondExpression)}
                        |  pop   %rcx
                        |  cmp   %rax, %rcx
                        |  mov   $0,   %rax
                        |  setge %al""".trimMargin()
                    Symbol.OR, Symbol.OR_ -> {
                        val label1 = generateLabel("or1_")
                        val label2 = generateLabel("or2_")
                        """${generateExpression(expression.firstExpression)}
                            |  cmp   $0,   %rax
                            |  je    $label1
                            |  mov   $1,   %rax
                            |  jmp   $label2
                            |$label1:
                            |${generateExpression(expression.secondExpression)}
                            |  cmp   $0,   %rax
                            |  mov   $0,   %rax
                            |  setne %al
                            |$label2:""".trimMargin()
                    }
                    Symbol.AND, Symbol.AND_ -> {
                        val label1 = generateLabel("and1_")
                        val label2 = generateLabel("and2_")
                        """${generateExpression(expression.firstExpression)}
                            |  cmp   $0,   %rax
                            |  jne   $label1
                            |  jmp   $label2
                            |$label1:
                            |${generateExpression(expression.secondExpression)}
                            |  cmp   $0,   %rax
                            |  mov   $0,   %rax
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