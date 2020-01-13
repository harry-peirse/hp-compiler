package com.aal.hp

import java.util.*

data class Program(val functionDeclaration: Function) {
    fun prettyPrint(): String {
        return functionDeclaration.prettyPrint()
    }
}

data class Function(val name: String, val blockItems: List<BlockItem>) {
    fun prettyPrint(): String {
        return "fun $name(): int {\n\t${blockItems.joinToString("\n\t") { it.prettyPrint() }}\n}"
    }
}

sealed class BlockItem {
    abstract fun prettyPrint(): String
    sealed class Statement : BlockItem() {
        data class Return(val expression: Expression) : Statement() {
            override fun prettyPrint() = "RETURN ${expression.prettyPrint()}"
        }

        data class ProxyExpression(val expression: Expression) : Statement() {
            override fun prettyPrint() = expression.prettyPrint()
        }

        data class Conditional(val condition: Expression, val statement: Statement, val elseStatement: Statement?) :
            Statement() {
            override fun prettyPrint() =
                "IF ${condition.prettyPrint()} THEN ${statement.prettyPrint()}${if (elseStatement != null) " ELSE ${elseStatement.prettyPrint()}" else ""}"
        }
    }

    data class Declaration(val variableName: String, val expression: Expression?) : BlockItem() {
        override fun prettyPrint() = "DECLARE $variableName = ${expression?.prettyPrint() ?: "<undefined>"}"
    }
}

sealed class Expression {
    abstract fun prettyPrint(): String

    data class Constant(val value: String) : Expression() {
        override fun prettyPrint() = value
    }

    data class Unary(val unaryOp: Token, val expression: Expression) : Expression() {
        override fun prettyPrint() = "(${unaryOp.value}${expression.prettyPrint()})"
    }

    data class Binary(val binaryOp: Token, val firstExpression: Expression, val secondExpression: Expression) :
        Expression() {
        override fun prettyPrint() =
            "(${firstExpression.prettyPrint()}${binaryOp.value}${secondExpression.prettyPrint()})"
    }

    data class Nested(val expression: Expression) : Expression() {
        override fun prettyPrint() = expression.prettyPrint()
    }

    data class Assign(val variableName: String, val assignmentToken: Token, val expression: Expression) :
        Expression() {
        override fun prettyPrint() = "$variableName${assignmentToken.value}${expression.prettyPrint()}"
    }

    data class Variable(val variableName: String) : Expression() {
        override fun prettyPrint() = variableName
    }

    data class Conditional(val condition: Expression, val expression: Expression, val elseExpression: Expression) :
        Expression() {
        override fun prettyPrint() =
            "IF ${condition.prettyPrint()} THEN ${expression.prettyPrint()} ELSE ${elseExpression.prettyPrint()}"
    }
}

class Ast {

    fun parseProgram(tokens: Queue<Token>) = Program(parseFunction(tokens))

    private fun parseFunction(tokens: Queue<Token>): Function {
        assert(tokens.poll().type == Keyword.INT)
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.OPEN_PARENTHESIS)
        assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
        assert(tokens.poll().type == Symbol.OPEN_BRACE)
        val blockItems = mutableListOf<BlockItem>()
        while (tokens.peek().type != Symbol.CLOSE_BRACE) {
            blockItems.add(parseBlockItem(tokens))
        }
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        assert(tokens.isEmpty())
        return Function(name.value, blockItems)
    }

    private fun parseBlockItem(tokens: Queue<Token>): BlockItem {
        val token = tokens.peek()
        val blockItem = when (token.type) {
            Keyword.RETURN -> {
                tokens.poll()
                BlockItem.Statement.Return(parseExpression(tokens))
            }
            Keyword.INT -> {
                tokens.poll()
                val variableName = tokens.poll()
                assert(variableName.type == IDENTIFIER)
                val expression = if (tokens.peek().type == Symbol.ASSIGN) {
                    tokens.poll()
                    parseExpression(tokens)
                } else null
                BlockItem.Declaration(variableName.value, expression)
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
            else -> BlockItem.Statement.ProxyExpression(parseExpression(tokens))
        }
        if (blockItem !is BlockItem.Statement.Conditional) {
            assert(tokens.poll().type == Symbol.SEMICOLON)
        }
        return blockItem
    }

    /*
<exp> ::= <id> "=" <exp> | <conditional-exp>
<conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
<logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
<logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
<equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
<relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
<additive-exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
<unary_op> ::= "!" | "~" | "-"
     */

    private fun parseNonBinaryExpression(tokens: Queue<Token>): Expression {
        val token = tokens.poll()
        return when {
            token.type == IDENTIFIER ->
                if (tokens.peek().type.isAssignment) {
                    val assignmentToken = tokens.poll()
                    Expression.Assign(token.value, assignmentToken, parseExpression(tokens))
                } else Expression.Variable(token.value)
            token.type == Literal.INT -> Expression.Constant(token.value)
            token.type.isUnary -> Expression.Unary(token, parseNonBinaryExpression(tokens))
            token.type == Symbol.OPEN_PARENTHESIS -> {
                val nested = Expression.Nested(parseExpression(tokens))
                assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                nested
            }
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
        println("Calculated expression: ${expression.prettyPrint()}")
        return expression
    }
}