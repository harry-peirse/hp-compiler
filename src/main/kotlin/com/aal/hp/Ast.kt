package com.aal.hp

import java.util.*

import com.aal.hp.Code.*

sealed class Code {

    data class Program(val functions: List<Function>): Code() {
        fun prettyPrint() = functions.joinToString("\n") { it.prettyPrint() }
    }

    data class Argument(val name: String, val type: String): Code() {
        fun prettyPrint() = "$name: $type"
    }

    data class Function(
        val name: String,
        val returnType: String,
        val arguments: List<Argument>,
        val blockItems: List<BlockItem>
    ): Code() {
        fun prettyPrint() =
            "FUNCTION $name(${arguments.joinToString(", ") { it.prettyPrint() }}): $returnType {\n\t${blockItems.joinToString(
                "\n\t"
            ) { it.prettyPrint() }}\n}"
    }

    sealed class BlockItem: Code() {
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

            data class Compound(val blockItems: List<BlockItem>) : Statement() {
                override fun prettyPrint() = "{\n\t\t${blockItems.joinToString("\n\t\t") { it.prettyPrint() }}\n\t}"
            }

            data class For(
                val initialization: Expression,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                init {
                    if (condition == Expression.Empty) {
                        condition = Expression.Constant("1", Literal.S64.value)
                    }
                }

                override fun prettyPrint() =
                    "FOR (${initialization.prettyPrint()} ; ${condition.prettyPrint()} ; ${increment.prettyPrint()}) ${statement.prettyPrint()}"
            }

            data class ForDeclaration(
                val declaration: Declaration,
                var condition: Expression,
                val increment: Expression,
                val statement: Statement
            ) : Statement() {
                init {
                    if (condition == Expression.Empty) {
                        condition = Expression.Constant("1", Literal.S64.value)
                    }
                }

                override fun prettyPrint() =
                    "FOR (${declaration.prettyPrint()} ; ${condition.prettyPrint()} ; ${increment.prettyPrint()}) ${statement.prettyPrint()}"
            }

            data class While(val condition: Expression, val statement: Statement) : Statement() {
                override fun prettyPrint() = "WHILE ${condition.prettyPrint()} DO ${statement.prettyPrint()}"
            }

            data class DoWhile(val statement: Statement, val condition: Expression) : Statement() {
                override fun prettyPrint() = "DO ${statement.prettyPrint()} WHILE ${condition.prettyPrint()}"
            }

            object Break : Statement() {
                override fun prettyPrint() = "BREAK"
            }

            object Continue : Statement() {
                override fun prettyPrint() = "CONTINUE"
            }
        }

        data class Declaration(val variableName: String, val type: String, val expression: Expression?) : BlockItem() {
            override fun prettyPrint() = "DECLARE $variableName: $type = ${expression?.prettyPrint() ?: "<undefined>"}"
        }
    }

    sealed class Expression: Code() {
        abstract fun prettyPrint(): String

        data class Constant(val value: String, val type: String) : Expression() {
            override fun prettyPrint() = "($value: $type)"
        }

        data class Cast(val string: String, val expression: Expression) : Expression() {
            override fun prettyPrint() = "(CAST($string)${expression.prettyPrint()})"
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

        object Empty : Expression() {
            override fun prettyPrint() = ""
        }

        data class FunctionCall(val name: String, val arguments: List<Expression>) : Expression() {
            override fun prettyPrint() = "$name(${arguments.joinToString(", ") { it.prettyPrint() }})"
        }
    }
}

class Ast {

    fun parseProgram(tokens: Queue<Token>): Program {
        val functions = mutableListOf<Code.Function>()
        do {
            functions.add(parseFunction(tokens))
        } while (tokens.size > 1)
        return Program(functions)
    }

    private fun parseFunction(tokens: Queue<Token>): Code.Function {
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.DOUBLE_COLON)
        val arguments = if (tokens.peek().type == Symbol.OPEN_PARENTHESIS) {
            tokens.poll()
            val arguments = mutableListOf<Argument>()
            while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
                assert(tokens.peek().type == IDENTIFIER)
                val argumentName = tokens.poll().value
                assert(tokens.poll().type == Symbol.COLON)
                assert(tokens.peek().type.isType)
                val argumentType = tokens.poll().type.value!!
                arguments.add(Argument(argumentName, argumentType))
                if (tokens.peek().type == Symbol.COMMA) tokens.poll()
            }
            assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
            assert(tokens.poll().type == Symbol.COLON)
            arguments
        } else listOf<Argument>()
        assert(tokens.peek().type.isType)
        val returnType = tokens.poll().type.value!!
        val blockItems = when (tokens.peek().type) {
            Symbol.OPEN_BRACE -> {
                tokens.poll()
                val blockItems = mutableListOf<BlockItem>()
                while (tokens.peek().type != Symbol.CLOSE_BRACE) {
                    blockItems.add(parseBlockItem(tokens))
                }
                tokens.poll()
                if (blockItems.isNotEmpty() && blockItems.last() is BlockItem.Statement.ProxyExpression) {
                    val last = blockItems.last() as BlockItem.Statement.ProxyExpression
                    blockItems.remove(last)
                    blockItems.add(BlockItem.Statement.Return(last.expression))
                }
                blockItems
            }
            Symbol.ASSIGN -> {
                tokens.poll()
                listOf(parseReturn(tokens))
            }
            else -> {
                throw IllegalStateException()
            }
        }
        return Function(name.value, returnType, arguments, blockItems)
    }

    private fun parseReturn(tokens: Queue<Token>): BlockItem.Statement.Return {
        val result = BlockItem.Statement.Return(parseExpression(tokens))
        if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
        return result
    }

    private fun parseBlockItem(tokens: Queue<Token>): BlockItem = when (tokens.peek().type) {
        Keyword.RETURN -> {
            tokens.poll()
            parseReturn(tokens)
        }
        Keyword.VAR -> {
            tokens.poll()
            val variableName = tokens.poll()
            assert(variableName.type == IDENTIFIER)
            assert(tokens.poll().type == Symbol.COLON)
            assert(tokens.peek().type == Keyword.S64 || tokens.peek().type == Keyword.F64)
            val type = tokens.poll().type.value!!
            val expression = if (tokens.peek().type == Symbol.ASSIGN) {
                tokens.poll()
                parseExpression(tokens)
            } else null
            val result = BlockItem.Declaration(variableName.value, type, expression)
            if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
            result
        }
        Keyword.IF -> {
            tokens.poll()
            val condition = parseExpression(tokens)
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
            val usingParentheses = tokens.peek().type == Symbol.OPEN_PARENTHESIS
            if (usingParentheses) tokens.poll()
            if (tokens.peek().type == Keyword.VAR) {
                val declaration = parseBlockItem(tokens) as BlockItem.Declaration
                val condition = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                val increment = parseExpression(tokens)
                if (usingParentheses) assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                val statement = parseBlockItem(tokens) as BlockItem.Statement
                BlockItem.Statement.ForDeclaration(declaration, condition, increment, statement)
            } else {
                val initialization = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                val condition = parseExpression(tokens)
                assert(tokens.poll().type == Symbol.SEMICOLON)
                val increment = parseExpression(tokens)
                if (usingParentheses) assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                val statement = parseBlockItem(tokens) as BlockItem.Statement
                BlockItem.Statement.For(initialization, condition, increment, statement)
            }
        }
        Keyword.WHILE -> {
            tokens.poll()
            val expression = parseExpression(tokens)
            val statement = parseBlockItem(tokens)
            BlockItem.Statement.While(expression, statement as BlockItem.Statement)
        }
        Keyword.DO -> {
            tokens.poll()
            val statement = parseBlockItem(tokens)
            assert(tokens.poll().type == Keyword.WHILE)
            val expression = parseExpression(tokens)
            BlockItem.Statement.DoWhile(statement as BlockItem.Statement, expression)
        }
        Keyword.BREAK -> {
            tokens.poll()
            BlockItem.Statement.Break
        }
        Keyword.CONTINUE -> {
            tokens.poll()
            BlockItem.Statement.Continue
        }
        else -> {
            val result = BlockItem.Statement.ProxyExpression(parseExpression(tokens))
            if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
            result
        }
    }

    private fun parseNonBinaryExpression(tokens: Queue<Token>): Expression {
        val token = tokens.peek()
        return when {
            token.type == IDENTIFIER -> {
                tokens.poll()
                if (tokens.peek().type.isAssignment) {
                    val assignmentToken = tokens.poll()
                    Expression.Assign(token.value, assignmentToken, parseExpression(tokens))
                } else if (tokens.peek().type == Symbol.OPEN_PARENTHESIS) {
                    tokens.poll()
                    val arguments = mutableListOf<Expression>()
                    while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
                        arguments.add(parseExpression(tokens))
                        if (tokens.peek().type == Symbol.COMMA) tokens.poll()
                    }
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    Expression.FunctionCall(token.value, arguments)
                } else Expression.Variable(token.value)
            }
            token.type is Literal -> {
                tokens.poll()
                Expression.Constant(token.value, token.type.value)
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
//        println("2] Inspecting token: $nextToken")
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
//        println("Parsed expression: $expression")
        return expression
    }
}