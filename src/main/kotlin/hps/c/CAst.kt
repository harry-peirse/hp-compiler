package hps.c

import hps.*
import hps.c.CCode.*
import hps.c.CCode.Function
import java.util.*

data class Type(val name: String, val pointer: Boolean)

sealed class CCode {
    abstract val token: Token
    abstract fun toC(): String

    data class Program(override val token: Token, val functions: List<Function>) : CCode() {
        override fun toC() = functions.joinToString("\n") { it.toC() }
    }

    data class Argument(override val token: Token, val type: Type, val name: Token) : CCode() {
        override fun toC(): String = "${type.name} ${name.value}"
    }

    data class Function(
        override val token: Token,
        val type: Type,
        val name: Token,
        val arguments: List<Argument>,
        val blockItems: List<BlockItem>
    ) : CCode() {
        override fun toC() =
            "${type.name} ${name.value}(${arguments.joinToString(", ") { it.toC() }}) {\n\t${blockItems.joinToString("\n\t") { it.toC() }}\n}"
    }

    sealed class BlockItem : CCode() {
        sealed class Statement : BlockItem() {
            data class Return(override val token: Token, val expression: Expression) : Statement() {
                override fun toC() = "return ${expression.toC()};"
            }

            data class ProxyExpression(val expression: Expression) : Statement() {
                override val token = expression.token
                override fun toC() = "${expression.toC()};"
            }

            data class Conditional(
                override val token: Token,
                val condition: Expression,
                val statement: Statement,
                val elseStatement: Statement?
            ) :
                Statement() {
                override fun toC() =
                    "if(${condition.toC()}) ${statement.toC()}${if (elseStatement != null) " else ${elseStatement.toC()}" else ""}"
            }

            data class Compound(override val token: Token, val blockItems: List<BlockItem>) : Statement() {
                override fun toC() = "{\n\t\t${blockItems.joinToString("\n\t\t") { it.toC() }}\n\t}"
            }

            data class For(
                override val token: Token,
                val initialization: Expression,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                init {
                    if (condition is Expression.Empty) {
                        condition = Expression.Constant(
                            Token(
                                condition.token.row,
                                condition.token.col,
                                condition.token.pos,
                                Literal.INT,
                                "1"
                            ), Type("int", false), "1"
                        )
                    }
                }

                override fun toC() =
                    "for(${initialization.toC()}; ${condition.toC()}; ${increment.toC()}) ${statement.toC()}"
            }

            data class ForDeclaration(
                override val token: Token,
                val declaration: Declaration,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                init {
                    if (condition is Expression.Empty) {
                        condition = Expression.Constant(
                            Token(
                                condition.token.row,
                                condition.token.col,
                                condition.token.pos,
                                Literal.INT,
                                "1"
                            ), Type("int", false), "1"
                        )
                    }
                }

                override fun toC() =
                    "for(${declaration.toC()} ${condition.toC()}; ${increment.toC()}) ${statement.toC()}"
            }

            data class While(override val token: Token, val condition: Expression, val statement: Statement) :
                Statement() {
                override fun toC() = "while(${condition.toC()}) ${statement.toC()}"
            }

            data class DoWhile(override val token: Token, val statement: Statement, val condition: Expression) :
                Statement() {
                override fun toC() = "do ${statement.toC()} while (${condition.toC()});"
            }

            data class Break(override val token: Token) : Statement() {
                override fun toC() = "break;"
            }

            data class Continue(override val token: Token) : Statement() {
                override fun toC() = "continue;"
            }
        }

        data class Declaration(
            override val token: Token,
            val type: Type,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC() =
                "${type.name} ${name.value} = ${expression?.toC() ?: ""};"
        }

        data class ArrayDeclaration(
            override val token: Token,
            val type: Type,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC() =
                "${type.name} ${name.value}[] = ${expression?.toC() ?: ""};"
        }
    }

    sealed class Expression : CCode() {
        data class Constant(override val token: Token, val type: Type, val value: String) : Expression() {
            override fun toC() = when (type.name) {
                "char" -> "'$value'"
                else -> value
            }
        }

        data class ArrayConstant(override val token: Token, val type: Type, val values: List<Expression>) :
            Expression() {
            override fun toC() = "{${values.joinToString(", ") { it.toC() }}}"
        }

        data class Unary(val unaryOp: Token, val expression: Expression) : Expression() {
            override val token = unaryOp
            override fun toC() = "(${unaryOp.value}${expression.toC()})"
        }

        data class Binary(
            override val token: Token,
            val binaryOp: Token,
            val firstExpression: Expression,
            val secondExpression: Expression
        ) :
            Expression() {
            override fun toC() =
                "(${firstExpression.toC()}${binaryOp.value}${secondExpression.toC()})"
        }

        data class Nested(val expression: Expression) : Expression() {
            override val token = expression.token
            override fun toC() = expression.toC()
        }

        data class Assign(
            val name: Token,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override val token = name
            override fun toC() = "${name.value} ${assignment.value} ${expression.toC()}"
        }

        data class AssignArrayIndex(
            val name: Token,
            val index: Expression,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override val token = name
            override fun toC() = "${name.value}[${index.toC()}] ${assignment.value} ${expression.toC()}"
        }

        data class Variable(val name: Token) : Expression() {
            override val token = name
            override fun toC() = name.value
        }

        data class ArrayVariable(val name: Token, val index: Expression) : Expression() {
            override val token = name
            override fun toC() = "${name.value}[${index.toC()}]"
        }

        data class Conditional(
            override val token: Token,
            val condition: Expression,
            val expression: Expression,
            val elseExpression: Expression
        ) :
            Expression() {
            override fun toC() =
                "${condition.toC()} ? ${expression.toC()} : ${elseExpression.toC()}"
        }

        data class Empty(override val token: Token) : Expression() {
            override fun toC() = ";"
        }

        data class FunctionCall(val name: Token, val arguments: List<Expression>) :
            Expression() {
            override val token = name
            override fun toC() = "${name.value}(${arguments.joinToString(", ") { it.toC() }})"
        }
    }
}

class Ast {

    fun parseProgram(tokens: Queue<Token>): Program {
        val functions = mutableListOf<Function>()
        do {
            functions.add(parseFunction(tokens))
        } while (tokens.peek().type != EOF)
        return Program(tokens.peek(), functions)
    }

    private fun parseFunction(tokens: Queue<Token>): Function {
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
            arguments.add(Argument(argumentType, Type(argumentType.value, false), argumentName))
            if (tokens.peek().type == Symbol.COMMA) tokens.poll()
        }
        assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
        assert(tokens.poll().type == Symbol.OPEN_BRACE)
        val blockItems = mutableListOf<BlockItem>()
        while (tokens.peek().type != Symbol.CLOSE_BRACE) {
            blockItems.add(parseBlockItem(tokens))
        }
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        return Function(returnType, Type(returnType.value, false), name, arguments, blockItems)
    }

    private fun parseBlockItem(tokens: Queue<Token>): BlockItem {
        val token = tokens.peek()
        val result = when (token.type) {
            Keyword.RETURN -> {
                tokens.poll()
                val result = BlockItem.Statement.Return(token, parseExpression(tokens))
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
                    if (pointer) BlockItem.ArrayDeclaration(token, Type(token.value, pointer), variableName, expression)
                    else BlockItem.Declaration(token, Type(token.value, pointer), variableName, expression)
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
                    token,
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
                BlockItem.Statement.Compound(token, blockItems)
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
                    BlockItem.Statement.ForDeclaration(token, declaration, condition, increment, statement)
                } else {
                    val initialization = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val condition = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val increment = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    val statement = parseBlockItem(tokens) as BlockItem.Statement
                    BlockItem.Statement.For(token, initialization, condition, increment, statement)
                }
            }
            Keyword.WHILE -> {
                tokens.poll()
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                val expression = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                val statement = parseBlockItem(tokens)
                BlockItem.Statement.While(token, expression, statement as BlockItem.Statement)
            }
            Keyword.DO -> {
                tokens.poll()
                val statement = parseBlockItem(tokens)
                assert(tokens.poll().type == Keyword.WHILE)
                assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
                val expression = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                BlockItem.Statement.DoWhile(token, statement as BlockItem.Statement, expression)
            }
            Keyword.BREAK -> {
                val result = BlockItem.Statement.Break(tokens.poll())
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            Keyword.CONTINUE -> {
                val result = BlockItem.Statement.Continue(tokens.poll())
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
            else -> {
                val result = BlockItem.Statement.ProxyExpression(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.SEMICOLON)
                result
            }
        }
        return result
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
                Expression.ArrayConstant(token, Type(token.type.value, true), values)
            }
            token.type is Literal -> {
                tokens.poll()
                Expression.Constant(token, Type(token.type.value, false), token.value)
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
            token.type == Symbol.SEMICOLON -> Expression.Empty(token)
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
                    op,
                    expression,
                    (nextExpression as Expression.Binary).firstExpression,
                    nextExpression.secondExpression
                )
            } else if (nextExpression is Expression.Binary && nextExpression.binaryOp.type.binaryPriority >= op.type.binaryPriority) Expression.Binary(
                nextExpression.binaryOp,
                nextExpression.binaryOp,
                Expression.Binary(op, op, expression, nextExpression.firstExpression),
                nextExpression.secondExpression
            ) else Expression.Binary(op, op, expression, nextExpression)
            nextToken = tokens.peek()
        }
        return expression
    }
}