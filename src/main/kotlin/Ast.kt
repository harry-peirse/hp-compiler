package com.aal.hp

import java.util.*

object Ast {
    data class Program(val functionDeclaration: Function) {
        override fun toString(): String {
            return "$functionDeclaration"
        }
    }

    data class Function(val name: String, val statements: List<Statement>) {
        override fun toString(): String {
            return "fun $name(): int {\n\t${statements.joinToString("\n\t")}}\n"
        }
    }

    sealed class Statement {
        data class Return(val expression: Expression) : Statement() {
            override fun toString() = "return $expression\n"
        }

        data class Declare(val variableName: String, val expression: Expression?) : Statement() {
            override fun toString() = "DECLARE $variableName = ${expression ?: "<undefined>"}\n"
        }

        data class ProxyExpression(val expression: Expression) : Statement() {
            override fun toString() = "$expression\n"
        }
    }

    sealed class Expression {
        data class Constant(val value: String) : Expression() {
            override fun toString() = value
        }

        data class Unary(val unaryOp: Token, val expression: Expression) : Expression() {
            override fun toString() = "(${unaryOp.value}$expression)"
        }

        data class Binary(val binaryOp: Token, val firstExpression: Expression, val secondExpression: Expression) :
            Expression() {
            override fun toString() = "($firstExpression${binaryOp.value}$secondExpression)"
        }

        data class Nested(val expression: Expression) : Expression() {
            override fun toString() = "$expression"
        }

        data class Assign(val variableName: String, val expression: Expression) : Expression() {
            override fun toString() = "$variableName = $expression"
        }

        data class Variable(val variableName: String) : Expression() {
            override fun toString() = variableName
        }
    }

    fun parseProgram(tokens: Queue<Token>) = Program(parseFunction(tokens))

    private fun parseFunction(tokens: Queue<Token>): Function {
        assert(tokens.poll().type == Keyword.INT)
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.OPEN_BRACKETS)
        assert(tokens.poll().type == Symbol.CLOSE_BRACKETS)
        assert(tokens.poll().type == Symbol.OPEN_BRACE)
        val statements = mutableListOf<Statement>()
        while (tokens.peek().type != Symbol.CLOSE_BRACE) {
            statements.add(parseStatement(tokens))
        }
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        assert(tokens.isEmpty())
        return Function(name.value, statements)
    }

    private fun parseStatement(tokens: Queue<Token>): Statement {
        val token = tokens.poll()
        val statement = when (token.type) {
            Keyword.RETURN -> Statement.Return(parseExpression(tokens))
            Keyword.INT -> {
                val variableName = tokens.poll()
                assert(variableName.type == IDENTIFIER)
                val expression = if (tokens.peek().type == Symbol.ASSIGN) {
                    parseExpression(tokens)
                } else null
                Statement.Declare(variableName.value, expression)
            }
            else -> Statement.ProxyExpression(parseExpression(tokens))
        }
        assert(tokens.poll().type == Symbol.SEMICOLON)
        return statement
    }

    private fun parseExpression(tokens: Queue<Token>): Expression {
        val token = tokens.poll()
        var expression = when {
            token.type == IDENTIFIER ->
                if (tokens.poll().type == Symbol.ASSIGN) Expression.Assign(token.value, parseExpression(tokens))
                else Expression.Variable(token.value)
            token.type == Literal.INT -> Expression.Constant(token.value)
            token.type.isUnary -> Expression.Unary(token, parseExpression(tokens))
            token.type == Symbol.OPEN_BRACKETS -> {
                val nested = Expression.Nested(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.CLOSE_BRACKETS)
                nested
            }
            else -> throw IllegalStateException()
        }

        var nextToken = tokens.peek()
        while (nextToken.type.isBinary) {
            val op = tokens.poll()
            val nextExpression = parseExpression(tokens)
            expression =
                if (nextExpression is Expression.Binary && nextExpression.binaryOp.type.binaryPriority >= op.type.binaryPriority) {
                    Expression.Binary(
                        nextExpression.binaryOp,
                        Expression.Binary(op, expression, nextExpression.firstExpression),
                        nextExpression.secondExpression
                    )
                } else {
                    Expression.Binary(op, expression, nextExpression)
                }
            nextToken = tokens.peek()
        }

        return expression
    }
}