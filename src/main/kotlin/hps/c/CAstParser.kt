package hps.c

import hps.*
import hps.c.CCode.*
import hps.c.CCode.Function
import java.util.*

sealed class CCode {
    abstract fun toC(): String

    data class Program(val functions: List<Function>) : CCode() {
        override fun toC() = functions.joinToString("\n") { it.toC() }
    }

    data class Argument(val type: Token, val name: Token) : CCode() {
        override fun toC(): String = "${type.value} ${name.value}"
    }

    sealed class Function : CCode() {

        abstract val type: Token
        abstract val name: Token
        abstract val arguments: List<Argument>

        data class Implementation(
            override val type: Token,
            override val name: Token,
            override val arguments: List<Argument>,
            val blockItems: List<BlockItem>
        ) : Function() {

            override fun toC() =
                "${type.value} ${name.value}(${arguments.joinToString(", ") { it.toC() }})\n{\n${blockItems.joinToString(
                    "\n"
                ) { it.toC() }}\n}\n"
        }

        data class External(
            override val type: Token,
            override val name: Token,
            override val arguments: List<Argument>
        ) : Function() {
            override fun toC() = ""
        }
    }

    sealed class BlockItem : CCode() {
        override fun toC() = toC(1)
        abstract fun toC(depth: Int, skipInitialTab: Boolean = false): String
        abstract fun blockItems(): List<BlockItem>
        abstract fun expressions(): List<Expression>

        sealed class Statement : BlockItem() {
            data class Return(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}return ${expression.toC()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
            }

            data class ProxyExpression(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${expression.toC()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
            }

            data class Conditional(
                val condition: Expression,
                val statement: Statement,
                val elseStatement: Statement?
            ) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}if(${condition.toC()})\n${statement.toC(depth + 1)}${if (elseStatement != null) "\n${"\t".repeat(
                        depth
                    )}else${if (elseStatement is Compound) "\n${elseStatement.toC(depth + 1)}" else " ${elseStatement.toC(
                        depth,
                        true
                    )}"}" else ""}"

                override fun blockItems() = listOfNotNull<BlockItem>(statement, elseStatement)
                override fun expressions() = listOf(condition)
            }

            data class Compound(val blockItems: List<BlockItem>) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}{\n${blockItems.joinToString("\n") { it.toC(depth + 1) }}\n${"\t".repeat(
                        depth
                    )}}"

                override fun blockItems() = blockItems
                override fun expressions() = emptyList<Expression>()
            }

            data class For(
                val initialization: Expression,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}for(${initialization.toC()}; ${if (condition is Expression.Empty) "1" else condition.toC()}; ${increment.toC()}) \n${statement.toC(
                        depth + 1
                    )}"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(initialization, condition, increment)
            }

            data class ForDeclaration(
                val declaration: Declaration,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}for(${declaration.toC(0)} ${if (condition is Expression.Empty) "1" else condition.toC()}; ${increment.toC()}) \n${statement.toC(
                        depth
                    )}"

                override fun blockItems() = listOf(declaration, statement)
                override fun expressions() = listOf(condition, increment)
            }

            data class While(val condition: Expression, val statement: Statement) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}while(${condition.toC()}) \n${statement.toC(depth + 1)}"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            data class DoWhile(val statement: Statement, val condition: Expression) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${"\t".repeat(depth)}do \n${statement.toC(depth + 1)} \n${if (skipInitialTab) "" else "\t".repeat(
                        depth
                    )}while (${condition.toC()});"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            object Break : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}break;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }

            object Continue : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}continue;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }
        }

        data class Declaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value} = ${expression?.toC()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }

        data class ArrayDeclaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value}[] = ${expression?.toC()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }
    }

    sealed class Expression : CCode() {

        abstract fun flattened(): List<Expression>

        data class Constant(val value: Token) : Expression() {
            override fun toC() = when (value.type) {
                Literal.CHAR -> "'${value.value}'"
                else -> value.value
            }

            override fun flattened() = listOf(this)
        }

        data class ArrayConstant(val type: Token, val values: List<Expression>) :
            Expression() {
            override fun toC() = "{${values.joinToString(", ") { it.toC() }}}"
            override fun flattened() = values.flatMap { it.flattened() } + this
        }

        data class Unary(val unaryOp: Token, val expression: Expression) : Expression() {
            override fun toC() = "(${unaryOp.value}${expression.toC()})"
            override fun flattened() = expression.flattened() + this
        }

        data class Binary(
            val binaryOp: Token,
            val firstExpression: Expression,
            val secondExpression: Expression
        ) :
            Expression() {
            override fun toC() = "(${firstExpression.toC()}${binaryOp.value}${secondExpression.toC()})"
            override fun flattened() = firstExpression.flattened() + secondExpression.flattened() + this
        }

        data class Nested(val expression: Expression) : Expression() {
            override fun toC() = expression.toC()
            override fun flattened() = expression.flattened() + this
        }

        data class Assign(
            val name: Token,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "${name.value} ${assignment.value} ${expression.toC()}"
            override fun flattened() = expression.flattened() + this
        }

        data class AssignArrayIndex(
            val name: Token,
            val index: Expression,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "${name.value}[${index.toC()}] ${assignment.value} ${expression.toC()}"
            override fun flattened() = index.flattened() + expression.flattened() + this
        }

        data class Variable(val name: Token) : Expression() {
            override fun toC() = name.value
            override fun flattened() = listOf(this)
        }

        data class ArrayVariable(val name: Token, val index: Expression) : Expression() {
            override fun toC() = "${name.value}[${index.toC()}]"
            override fun flattened() = index.flattened() + this
        }

        data class Conditional(
            val condition: Expression,
            val expression: Expression,
            val elseExpression: Expression
        ) :
            Expression() {
            override fun toC() = "${condition.toC()} ? ${expression.toC()} : ${elseExpression.toC()}"
            override fun flattened() =
                condition.flattened() + expression.flattened() + elseExpression.flattened() + this
        }

        object Empty : Expression() {
            override fun toC() = ""
            override fun flattened() = listOf(this)
        }

        data class FunctionCall(val name: Token, val arguments: List<Expression>) : Expression() {
            override fun toC() = "${name.value}(${arguments.joinToString(", ") { it.toC() }})"
            override fun flattened() = arguments.flatMap { it.flattened() } + this
        }
    }
}

class Ast {

    fun parseProgram(tokens: Queue<Token>): Program {
        val functions = mutableListOf<Function>()
        do {
            functions.add(parseFunction(tokens))
        } while (tokens.peek().type != EOF)
        return Program(functions)
    }

    private fun parseFunction(tokens: Queue<Token>): Function {
        val external = if (tokens.peek().type == Keyword.EXTERNAL) {
            tokens.poll()
            true
        } else false
        val returnType = tokens.poll()
        assert(returnType.type.isType)
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
        val arguments = mutableListOf<Argument>()
        while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
            val argumentType = tokens.poll()
            assert(argumentType.type.isType)
            val argumentName = tokens.poll()
            arguments.add(Argument(argumentType, argumentName))
            if (tokens.peek().type == Symbol.COMMA) tokens.poll()
        }
        assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
        if (!external) {
            assert(tokens.poll().type == Symbol.OPEN_BRACE)
            val blockItems = mutableListOf<BlockItem>()
            while (tokens.peek().type != Symbol.CLOSE_BRACE) {
                blockItems.add(parseBlockItem(tokens))
            }
            assert(tokens.poll().type == Symbol.CLOSE_BRACE)
            return Function.Implementation(returnType, name, arguments, blockItems)
        } else return Function.External(returnType, name, arguments)
    }

    private fun parseBlockItem(tokens: Queue<Token>): BlockItem {
        val token = tokens.peek()
        return when (token.type) {
            Keyword.RETURN -> {
                tokens.poll()
                val result = BlockItem.Statement.Return(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            Keyword.INT, Keyword.FLOAT, Keyword.CHAR -> {
                tokens.poll()
                val pointer = if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
                    tokens.poll()
                    assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
                    true
                } else false
                val variableName = tokens.poll()
                assert(variableName.type == IDENTIFIER)
                val expression = if (tokens.peek().type == Symbol.ASSIGN) {
                    tokens.poll()
                    parseExpression(tokens)
                } else null
                val result =
                    if (pointer) BlockItem.ArrayDeclaration(token, variableName, expression)
                    else BlockItem.Declaration(token, variableName, expression)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            Keyword.IF -> {
                tokens.poll()
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                val condition = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                val firstExpression = parseBlockItem(tokens)
                val secondExpression = if (tokens.peek().type == Keyword.ELSE) {
                    tokens.poll()
                    parseBlockItem(tokens)
                } else null
                BlockItem.Statement.Conditional(
                    condition,
                    firstExpression as BlockItem.Statement,
                    secondExpression as BlockItem.Statement?
                )
            }
            Symbol.OPEN_BRACE -> {
                tokens.poll()
                val blockItems = mutableListOf<BlockItem>()
                while (tokens.peek().type != Symbol.CLOSE_BRACE) {
                    blockItems.add(parseBlockItem(tokens))
                }
                tokens.poll()
                BlockItem.Statement.Compound(blockItems)
            }
            Keyword.FOR -> {
                tokens.poll()
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                if (tokens.peek().type.isType) {
                    val declaration = parseBlockItem(tokens) as BlockItem.Declaration
                    val condition = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val increment = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    val statement = parseBlockItem(tokens) as BlockItem.Statement
                    BlockItem.Statement.ForDeclaration(declaration, condition, increment, statement)
                } else {
                    val initialization = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val condition = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val increment = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    val statement = parseBlockItem(tokens) as BlockItem.Statement
                    BlockItem.Statement.For(initialization, condition, increment, statement)
                }
            }
            Keyword.WHILE -> {
                tokens.poll()
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                val expression = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                val statement = parseBlockItem(tokens)
                BlockItem.Statement.While(expression, statement as BlockItem.Statement)
            }
            Keyword.DO -> {
                tokens.poll()
                val statement = parseBlockItem(tokens)
                assert(tokens.poll().type == Keyword.WHILE)
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                val expression = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                BlockItem.Statement.DoWhile(statement as BlockItem.Statement, expression)
            }
            Keyword.BREAK -> {
                tokens.poll()
                val result = BlockItem.Statement.Break
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            Keyword.CONTINUE -> {
                tokens.poll()
                val result = BlockItem.Statement.Continue
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            else -> {
                val result = BlockItem.Statement.ProxyExpression(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
        }
    }

    private fun parseNonBinaryExpression(tokens: Queue<Token>): Expression {
        val token = tokens.peek()
        return when {
            token.type == IDENTIFIER -> {
                tokens.poll()
                if (tokens.peek().type.isAssignment) {
                    val assignmentToken = tokens.poll()
                    Expression.Assign(token, assignmentToken, parseExpression(tokens))
                } else if (tokens.peek().type == Symbol.OPEN_PARENTHESIS) {
                    tokens.poll()
                    val arguments = mutableListOf<Expression>()
                    while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
                        arguments.add(parseExpression(tokens))
                        if (tokens.peek().type == Symbol.COMMA) tokens.poll()
                    }
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    Expression.FunctionCall(token, arguments)
                } else if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
                    tokens.poll()
                    val index = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
                    if (tokens.peek().type.isAssignment) {
                        val assignmentToken = tokens.poll()
                        Expression.AssignArrayIndex(token, index, assignmentToken, parseExpression(tokens))
                    } else Expression.ArrayVariable(token, index)
                } else Expression.Variable(token)
            }
            token.type == Symbol.OPEN_SQUARE_BRACKET -> {
                val values = mutableListOf<Expression>()
                tokens.poll()
                while (tokens.peek().type != Symbol.CLOSE_SQUARE_BRACKET) {
                    values.add(parseExpression(tokens))
                    if (tokens.peek().type != Symbol.CLOSE_SQUARE_BRACKET) {
                        assert(tokens.poll().type == Symbol.COMMA)
                    }
                }
                tokens.poll()
                Expression.ArrayConstant(token, values)
            }
            token.type is Literal -> {
                tokens.poll()
                Expression.Constant(token)
            }
            token.type.isUnary -> {
                tokens.poll()
                Expression.Unary(token, parseNonBinaryExpression(tokens))
            }
            token.type == Symbol.OPEN_PARENTHESIS -> {
                tokens.poll()
                val nested = Expression.Nested(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                nested
            }
            token.type == Symbol.SEMICOLON -> Expression.Empty
            else -> throw IllegalStateException("Unexpected token: $token")
        }
    }

    private fun parseExpression(tokens: Queue<Token>, binaryPriority: Int = Int.MAX_VALUE): Expression {
        var expression = parseNonBinaryExpression(tokens)
        var nextToken = tokens.peek()
        while (nextToken.type.isBinary && nextToken.type.binaryPriority <= binaryPriority) {
            val op = tokens.poll()
            val nextExpression = parseExpression(tokens, op.type.binaryPriority)
            expression = if (op.type == Symbol.QUESTION_MARK) { // ternary operator
                assert(nextExpression is Expression.Binary && nextExpression.binaryOp.type == Symbol.COLON)
                Expression.Conditional(
                    expression,
                    (nextExpression as Expression.Binary).firstExpression,
                    nextExpression.secondExpression
                )
            } else if (nextExpression is Expression.Binary && nextExpression.binaryOp.type.binaryPriority >= op.type.binaryPriority) Expression.Binary(
                nextExpression.binaryOp,
                Expression.Binary(op, expression, nextExpression.firstExpression),
                nextExpression.secondExpression
            ) else Expression.Binary(op, expression, nextExpression)
            nextToken = tokens.peek()
        }
        return expression
    }
}