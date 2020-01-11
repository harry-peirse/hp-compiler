package com.aal.hp

import java.util.*

object Ast {
    data class Program(val functionDeclaration: Function) {
        override fun toString(): String {
            return "$functionDeclaration"
        }
    }

    data class Function(val name: String, val statement: Statement) {
        override fun toString(): String {
            return "fun $name(): int = $statement"
        }
    }

    sealed class Statement {
        data class Return(val expression: Expression) : Statement() {
            override fun toString() = "$expression"
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
    }

    fun parseProgram(tokens: Queue<Token>) = Program(parseFunction(tokens))

    private fun parseFunction(tokens: Queue<Token>): Function {
        assert(tokens.poll().type == Keyword.INT)
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.OPEN_BRACKETS)
        assert(tokens.poll().type == Symbol.CLOSE_BRACKETS)
        assert(tokens.poll().type == Symbol.OPEN_BRACE)
        val function = Function(name.value, parseStatement(tokens))
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        assert(tokens.isEmpty())
        return function
    }

    private fun parseStatement(tokens: Queue<Token>): Statement {
        assert(tokens.poll().type == Keyword.RETURN)
        val statement = Statement.Return(parseExpression(tokens))
        assert(tokens.poll().type == Symbol.SEMICOLON)
        return statement
    }

    private fun parseExpression(tokens: Queue<Token>): Expression {
        val token = tokens.poll()
        var expression = when {
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