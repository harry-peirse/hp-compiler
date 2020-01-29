package hps

import hps.HPCode.*
import java.util.*

class Ast {

    private fun parseArguments(tokens: Queue<Token>): List<Argument> {
        val arguments = mutableListOf<Argument>()
        if (tokens.peek().type == Symbol.OPEN_PARENTHESIS) {
            tokens.poll()
            while (tokens.peek().type != Symbol.CLOSE_PARENTHESIS) {
                val argumentName = tokens.poll()
                assert(argumentName.type == IDENTIFIER)
                assert(tokens.poll().type == Symbol.COLON)
                val argumentType = tokens.poll()
                assert(argumentType.type.isType)
                val isArray = if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
                    tokens.poll()
                    assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
                    true
                } else false
                arguments.add(Argument(argumentType, argumentName, isArray))
                if (tokens.peek().type == Symbol.COMMA) tokens.poll()
            }
            assert(tokens.poll().type == Symbol.CLOSE_PARENTHESIS)
        }
        return arguments
    }

    fun parseProgram(tokens: Queue<Token>): Program {
        val functions = mutableListOf<HPCode.Func>()
        val structs = mutableListOf<Struct>()
        do {
            if (tokens.peek().type == Keyword.STRUCT) {
                structs.add(parseStruct(tokens))
            } else {
                functions.add(parseFunction(tokens))
            }
        } while (tokens.peek().type != EOF)
        return Program(structs, functions)
    }

    private fun parseStruct(tokens: Queue<Token>): Struct {
        assert(tokens.poll().type == Keyword.STRUCT)
        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        return Struct(name, parseArguments(tokens))
    }

    private fun parseFunction(tokens: Queue<Token>): HPCode.Func {
        val external = if (tokens.peek().type == Keyword.EXTERNAL) {
            tokens.poll()
            true
        } else false

        val name = tokens.poll()
        assert(name.type == IDENTIFIER)
        assert(tokens.poll().type == Symbol.DOUBLE_COLON)

        val hasBrackets = tokens.peek().type == Symbol.OPEN_PARENTHESIS
        val arguments = parseArguments(tokens)

        val returnType = when {
            hasBrackets && tokens.peek().type == Symbol.MINUS_RIGHT_ARROW -> {
                tokens.poll()
                tokens.poll()
            }
            tokens.peek().type.isType -> {
                tokens.poll()
            }
            else -> {
                name.copy(type = Keyword.VOID, value = "void")
            }
        }
        assert(returnType.type.isType)
        val isArray = if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
            tokens.poll()
            assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
            true
        } else false

        return if (!external) {
            val blockItems = mutableListOf<BlockItem>()
            if (tokens.peek().type == Symbol.EQUALS) {
                tokens.poll()
                val blockItem = parseBlockItem(tokens)
                if (returnType.type != Keyword.VOID && blockItem is BlockItem.Statement.ProxyExpression) {
                    blockItems.add(BlockItem.Statement.Return(blockItem.expression))
                } else {
                    blockItems.add(blockItem)
                }
            } else {
                assert(tokens.poll().type == Symbol.OPEN_BRACE)
                while (tokens.peek().type != Symbol.CLOSE_BRACE) {
                    blockItems.add(parseBlockItem(tokens))
                }
                assert(tokens.poll().type == Symbol.CLOSE_BRACE)
            }
            HPCode.Func.Implementation(returnType, isArray, name, arguments, blockItems)
        } else HPCode.Func.External(returnType, isArray, name, arguments)
    }

    private fun parseBlockItem(tokens: Queue<Token>): BlockItem {
        val token = tokens.peek()
        return when (token.type) {
            Keyword.RETURN -> {
                tokens.poll()
                val result = BlockItem.Statement.Return(parseExpression(tokens))
                if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
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
                val expression = if (tokens.peek().type == Symbol.EQUALS) {
                    tokens.poll()
                    parseExpression(tokens)
                } else null
                val result =
                    if (pointer) BlockItem.ArrayDeclaration(token, variableName, expression)
                    else BlockItem.Declaration(token, variableName, expression)
                if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
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
                if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
                result
            }
            Keyword.CONTINUE -> {
                tokens.poll()
                val result = BlockItem.Statement.Continue
                if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
                result
            }
            IDENTIFIER -> {
                val first = tokens.poll()
                val second = tokens.poll()
                val third = tokens.poll()

                if ((second.type == IDENTIFIER) || (second.type == Symbol.OPEN_SQUARE_BRACKET && third.type == Symbol.CLOSE_SQUARE_BRACKET && tokens.poll().type == IDENTIFIER)) {
                    (tokens as ArrayDeque).apply {
                        push(third)
                        push(second)
                    }

                    val pointer = if (tokens.peek().type == Symbol.OPEN_SQUARE_BRACKET) {
                        tokens.poll()
                        assert(tokens.poll().type == Symbol.CLOSE_SQUARE_BRACKET)
                        true
                    } else false
                    val variableName = tokens.poll()
                    assert(variableName.type == IDENTIFIER)
                    val expression = if (tokens.peek().type == Symbol.EQUALS) {
                        tokens.poll()
                        parseExpression(tokens)
                    } else null
                    val result =
                        if (pointer) BlockItem.ArrayDeclaration(token, variableName, expression)
                        else BlockItem.Declaration(token, variableName, expression)
                    if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
                    result
                } else {
                    (tokens as ArrayDeque).apply {
                        push(third)
                        push(second)
                        push(first)
                    }
                    val result = BlockItem.Statement.ProxyExpression(parseExpression(tokens))
                    if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
                    result
                }
            }
            else -> {
                val result = BlockItem.Statement.ProxyExpression(parseExpression(tokens))
                if (tokens.peek().type == Symbol.SEMICOLON) tokens.poll()
                result
            }
        }
    }

    private fun parseNonBinaryExpression(tokens: Queue<Token>): Expression {
        val token = tokens.peek()
        return when {
            token.type == IDENTIFIER -> {
                tokens.poll()
                if (tokens.peek().type.isPostfix) {
                    Expression.Unary(tokens.poll(), Expression.Constant(token), true)
                } else if (tokens.peek().type.isAssignment) {
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
                Expression.ArrayConstant(values, "char") // TODO: don't assume all arrays are chars!!!
            }
            token.type is Literal -> {
                tokens.poll()
                Expression.Constant(token)
            }
            token.type.isUnary -> {
                tokens.poll()
                Expression.Unary(token, parseNonBinaryExpression(tokens), false)
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