package hps

import hps.Ast.*
import hps.Ast.Function
import java.util.*

sealed class Ast {
    abstract fun toC(): String

    data class Program(val structs: List<Struct>, val functions: List<Function>) : Ast() {
        override fun toC() = structs.joinToString("\n") { it.toC() } + "\n\n" + functions.joinToString("\n") { it.toC() }
    }

    data class Argument(val type: Token, val name: Token) : Ast() {
        override fun toC(): String = if (type.type == Keyword.S64) {
            "int ${name.value}"
        } else {
            "${type.value} ${name.value}"
        }
    }

    data class Function(
        val type: Token,
        val name: Token,
        val arguments: List<Argument>,
        val blockItems: List<BlockItem>
    ) : Ast() {
        override fun toC() = if (type.type == Keyword.S64)
            "int ${name.value}(${arguments.joinToString(", ") { it.toC() }})\n{\n${blockItems.joinToString("\n") { it.toC() }}\n}\n"
        else "${type.value} ${name.value}(${arguments.joinToString(", ") { it.toC() }})\n{\n${blockItems.joinToString("\n") { it.toC() }}\n}\n"
    }

    data class Struct(
        val name: Token,
        val members: List<StructMember>
    ) : Ast() {
        override fun toC() =
            "typedef struct _${name.value} {\n${members.joinToString("\n") { it.toC() }}\n} ${name.value};"
    }

    data class StructMember(
        val name: Token,
        val typeName: Token,
        val pointer: Boolean,
        val arraySize: Int
    ) : Ast() {
        override fun toC() = if(typeName.type == Keyword.S64)
            "\tint ${name.value}${if (pointer) "[$arraySize]" else ""};"
         else
            "\t${typeName.value} ${name.value}${if (pointer) "[$arraySize]" else ""};"
    }

    sealed class BlockItem : Ast() {
        override fun toC() = toC(1)
        abstract fun toC(depth: Int, skipInitialTab: Boolean = false): String

        sealed class Statement : BlockItem() {
            data class Return(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}return ${expression.toC()};"
            }

            data class ProxyExpression(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${expression.toC()};"
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
            }

            data class Compound(val blockItems: List<BlockItem>) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}{\n${blockItems.joinToString("\n") { it.toC(depth + 1) }}\n${"\t".repeat(
                        depth
                    )}}"
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
            }

            data class While(val condition: Expression, val statement: Statement) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}while(${condition.toC()}) \n${statement.toC(depth + 1)}"
            }

            data class DoWhile(val statement: Statement, val condition: Expression) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${"\t".repeat(depth)}do \n${statement.toC(depth + 1)} \n${if (skipInitialTab) "" else "\t".repeat(
                        depth
                    )}while (${condition.toC()});"
            }

            object Break : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}break;"
            }

            object Continue : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}continue;"
            }
        }

        data class Declaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                if (type.type == Keyword.S64)
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}int ${name.value}${if(expression != null) " = ${expression.toC()}" else ""};"
                else
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value}${if(expression != null) " = ${expression.toC()}" else ""};"
        }

        data class ArrayDeclaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                if (type.type == Keyword.S64)
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}int ${name.value}[]${if(expression != null) " = ${expression.toC()}" else ""};"
                else
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value}[]${if(expression != null) " = ${expression.toC()}" else ""};"
        }
    }

    sealed class Expression : Ast() {
        data class Constant(val value: Token) : Expression() {
            override fun toC() = when (value.type) {
                Literal.CHAR -> "'${value.value}'"
                else -> value.value
            }
        }

        data class ArrayConstant(val type: Token, val values: List<Expression>) :
            Expression() {
            override fun toC() = "{${values.joinToString(", ") { it.toC() }}}"
        }

        data class Unary(val unaryOp: Token, val expression: Expression) : Expression() {
            override fun toC() = "(${unaryOp.value}${expression.toC()})"
        }

        data class Binary(
            val binaryOp: Token,
            val firstExpression: Expression,
            val secondExpression: Expression
        ) :
            Expression() {
            override fun toC() = "(${firstExpression.toC()}${binaryOp.value}${secondExpression.toC()})"
        }

        data class Nested(val expression: Expression) : Expression() {
            override fun toC() = expression.toC()
        }

        data class Assign(
            val name: Token,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "${name.value} ${assignment.value} ${expression.toC()}"
        }

        data class AssignArrayIndex(
            val name: Token,
            val index: Expression,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "${name.value}[${index.toC()}] ${assignment.value} ${expression.toC()}"
        }

        data class Variable(val name: Token) : Expression() {
            override fun toC() = if (name.type == Keyword.S64) "int" else name.value
        }

        data class ArrayVariable(val name: Token, val index: Expression) : Expression() {
            override fun toC() = "${name.value}[${index.toC()}]"
        }

        data class Conditional(
            val condition: Expression,
            val expression: Expression,
            val elseExpression: Expression
        ) :
            Expression() {
            override fun toC() = "${condition.toC()} ? ${expression.toC()} : ${elseExpression.toC()}"
        }

        object Empty : Expression() {
            override fun toC() = ""
        }

        data class FunctionCall(val name: Token, val arguments: List<Expression>) : Expression() {
            override fun toC() = "${name.value}(${arguments.joinToString(", ") { it.toC() }})"
        }
    }
}

object Parser {
    fun parseProgram(tokens: ArrayDeque<Token>): Program {
        val functions = mutableListOf<Function>()
        val structs = mutableListOf<Struct>()
        do {
            val name = tokens.poll()
            assert(name.type == IDENTIFIER)

            val doubleColon = tokens.poll()
            assert(doubleColon.type == Symbol.DOUBLE_COLON)

            if (tokens.peek().type == Keyword.STRUCT) {
                tokens.addFirst(doubleColon)
                tokens.addFirst(name)
                structs.add(parseStruct(tokens))
            } else {
                tokens.addFirst(doubleColon)
                tokens.addFirst(name)
                functions.add(parseFunction(tokens))
            }
        } while (tokens.peek().type != EOF)
        return Program(structs, functions)
    }

    private fun parseStruct(tokens: ArrayDeque<Token>): Struct {
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)

        assert(tokens.poll().type == Symbol.DOUBLE_COLON)
        assert(tokens.poll().type == Keyword.STRUCT)
        assert(tokens.poll().type == Symbol.OPEN_BRACE)

        val structMembers = mutableListOf<StructMember>()
        while (tokens.peek().type != Symbol.CLOSE_BRACE) {
            if (tokens.peek().type == Symbol.SEMICOLON) {
                tokens.poll()
            } else {
                structMembers.add(parseStructMember(tokens))
            }
        }
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        return Struct(name, structMembers)
    }

    private fun parseStructMember(tokens: ArrayDeque<Token>): StructMember {
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.COLON)
        val type = tokens.poll()
        assert(type.type.isType)

        val pointer = tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET
        val arraySize = if (pointer) {
            tokens.poll()
            val arraySize = tokens.poll()
            assert(arraySize.type == Literal.INT)
            assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
            arraySize.value.toInt()
        } else 0

        return StructMember(name, type, pointer, arraySize)
    }

    private fun parseFunction(tokens: ArrayDeque<Token>): Function {
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)

        assert(tokens.poll().type == Symbol.DOUBLE_COLON)

        val arguments: List<Argument> = if (tokens.peek().type == Symbol.OPEN_PARENTHESIS) {
            tokens.poll()
            val arguments = mutableListOf<Argument>()
            while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
                val argumentName = tokens.poll()
                assert(tokens.poll().type == Symbol.COLON)
                val argumentType = tokens.poll()
                assert(argumentType.type.isType)
                arguments.add(Argument(argumentType, argumentName))
                if (tokens.peek().type == Symbol.COMMA) tokens.poll()
            }
            assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
            arguments
        } else emptyList()

        val returnType = if (tokens.peek().type == Symbol.ARROW) {
            tokens.poll()

            val returnType = tokens.poll()
            assert(returnType.type.isType)
            returnType
        } else Token(name.row, name.col, name.pos, Keyword.VOID, "")

        assert(tokens.poll().type == Symbol.OPEN_BRACE)
        val blockItems = mutableListOf<BlockItem>()
        while (tokens.peek().type != Symbol.CLOSE_BRACE) {
            blockItems.add(parseBlockItem(tokens))
        }
        assert(tokens.poll().type == Symbol.CLOSE_BRACE)
        return Function(returnType, name, arguments, blockItems)
    }

    private fun parseBlockItem(tokens: ArrayDeque<Token>): BlockItem {
        val token = tokens.peek()
        return when (token.type) {
            Keyword.RETURN -> {
                tokens.poll()
                BlockItem.Statement.Return(parseExpression(tokens))
            }
            IDENTIFIER -> {
                val variableName = tokens.poll()
                if (tokens.peek().type == Symbol.COLON) {
                    tokens.poll()
                    val variableType = tokens.poll()
                    assert(variableType.type.isType)
                    val pointer = if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
                        tokens.poll()
                        assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
                        true
                    } else false

                    val expression = if (tokens.peek().type == Symbol.ASSIGN) {
                        tokens.poll()
                        parseExpression(tokens)
                    } else null
                    if (pointer) BlockItem.ArrayDeclaration(variableType, variableName, expression)
                    else BlockItem.Declaration(variableType, variableName, expression)
                } else {
                    tokens.addFirst(variableName)
                    BlockItem.Statement.ProxyExpression(parseExpression(tokens))
                }
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

                val usingParenthesis = tokens.peek().type == Symbol.OPEN_PARENTHESIS
                if (usingParenthesis) {
                    tokens.poll()
                }

                if (tokens.peek().type.isType) {
                    val declaration = parseBlockItem(tokens) as BlockItem.Declaration
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val condition = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val increment = parseExpression(tokens)
                    if (usingParenthesis) {
                        assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    }
                    val statement = parseBlockItem(tokens) as BlockItem.Statement
                    BlockItem.Statement.ForDeclaration(declaration, condition, increment, statement)
                } else {
                    val initialization = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val condition = parseExpression(tokens)
                    assert(tokens.poll().type == Symbol.SEMICOLON)
                    val increment = parseExpression(tokens)
                    if (usingParenthesis) {
                        assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                    }
                    val statement = parseBlockItem(tokens) as BlockItem.Statement
                    BlockItem.Statement.For(initialization, condition, increment, statement)
                }
            }
            Keyword.WHILE -> {
                tokens.poll()
                val usingParenthesis = tokens.peek().type == Symbol.OPEN_PARENTHESIS
                if (usingParenthesis) {
                    tokens.poll()
                }
                val expression = parseExpression(tokens)
                if (usingParenthesis) {
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                }
                val statement = parseBlockItem(tokens)
                BlockItem.Statement.While(expression, statement as BlockItem.Statement)
            }
            Keyword.DO -> {
                tokens.poll()
                val statement = parseBlockItem(tokens)
                assert(tokens.poll().type == Keyword.WHILE)
                val usingParenthesis = tokens.peek().type == Symbol.OPEN_PARENTHESIS
                if (usingParenthesis) {
                    tokens.poll()
                }
                val expression = parseExpression(tokens)
                if (usingParenthesis) {
                    assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
                }
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
                BlockItem.Statement.ProxyExpression(parseExpression(tokens))
            }
        }
    }

    private fun parseNonBinaryExpression(tokens: ArrayDeque<Token>): Expression {
        val token = tokens.peek()
        return when {
            token.type == IDENTIFIER -> {
                tokens.poll()
                if (tokens.peek().type.isAssignment) {
                    val assignmentToken = tokens.poll()
                    Expression.Assign(
                        token, assignmentToken,
                        parseExpression(tokens)
                    )
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
                        Expression.AssignArrayIndex(
                            token, index, assignmentToken,
                            parseExpression(tokens)
                        )
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

    private fun parseExpression(tokens: ArrayDeque<Token>, binaryPriority: Int = Int.MAX_VALUE): Expression {
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