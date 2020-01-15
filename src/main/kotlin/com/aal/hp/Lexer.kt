package com.aal.hp

import java.io.File
import java.util.*

interface TokenType {
    val value: String?
    val category: String
    val isUnary: Boolean
    val isBinary: Boolean
    val binaryPriority: Int
    val isAssignment: Boolean
    val isPostfix: Boolean
    val isType: Boolean
}

enum class Keyword(override val value: String, override val isType: Boolean = false) : TokenType {
    RETURN("return"),
    IF("if"),
    ELSE("else"),
    FOR("for"),
    DO("do"),
    WHILE("while"),
    BREAK("break"),
    CONTINUE("continue"),
    VAR("var"),
    VOID("void", true),
    S64("s64", true), // int
    F64("f64", true); // float

    override val category = "Keyword"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
    override val isAssignment: Boolean = false
    override val isPostfix: Boolean = false
}

enum class Symbol(
    override val value: String,
    override val binaryPriority: Int = -1,
    override val isUnary: Boolean = false,
    override val isBinary: Boolean = false,
    override val isAssignment: Boolean = false,
    override val isPostfix: Boolean = false,
    override val isType: Boolean = false
) : TokenType {
    OPEN_PARENTHESIS("("),
    CLOSE_PARENTHESIS(")"),
    OPEN_BRACE("{"),
    CLOSE_BRACE("}"),
    OPEN_SQUARE_BRACKET("["),
    CLOSE_SQUARE_BRACKET("]"),
    SINGLE_QUOTE("'"),
    DOUBLE_QUOTE("\""),
    SEMICOLON(";"),
    PERIOD("."),
    COMMA(","),
    ARROW("->"),
    DOUBLE_COLON("::"),
    QUESTION_MARK("?", 13, isBinary = true),
    COLON(":", 13, isBinary = true),
    MINUS("-", 4, isBinary = true, isUnary = true),
    TILDA("~", isUnary = true),
    BANG("!", isUnary = true),
    PLUS("+", 4, isBinary = true, isUnary = true),
    ASTERISK("*", 3, isBinary = true, isUnary = true),
    SLASH("/", 3, isBinary = true),
    BITWISE_AND("&", 8, isBinary = true),
    BITWISE_OR("|", 10, isBinary = true),
    BITWISE_XOR("^", 9, isBinary = true),
    BITWISE_SHIFT_LEFT("<<", 5, isBinary = true),
    BITWISE_SHIFT_RIGHT(">>", 5, isBinary = true),
    MODULUS("%", 3, isBinary = true, isUnary = true),
    AND("&&", 11, isBinary = true),
    AND_("and", 11, isBinary = true),
    OR("||", 12, isBinary = true),
    OR_("or", 12, isBinary = true),
    EQUAL("==", 7, isBinary = true),
    NOT_EQUAL("!=", 7, isBinary = true),
    LESS_THAN("<", 6, isBinary = true),
    LESS_THAN_OR_EQUAL_TO("<=", 6, isBinary = true),
    GREATER_THAN(">", 6, isBinary = true),
    GREATER_THAN_OR_EQUAL_TO(">=", 6, isBinary = true),
    ASSIGN("=", isAssignment = true),
    PLUS_ASSIGN("+=", isAssignment = true),
    MINUS_ASSIGN("-=", isAssignment = true),
    TIMES_ASSIGN("*=", isAssignment = true),
    DIVIDE_ASSIGN("/=", isAssignment = true),
    MODULUS_ASSIGN("%=", isAssignment = true),
    BITWISE_SHIFT_LEFT_ASSIGN("<<="),
    BITWISE_SHIFT_RIGHT_ASSIGN(">>="),
    BITWISE_AND_ASSIGN("&="),
    BITWISE_OR_ASSIGN("|="),
    BITWISE_XOR_ASSIGN("^="),
    INCREMENT("++", isUnary = true, isPostfix = true),
    DECREMENT("--", isUnary = true, isPostfix = true);

    override val category = "Symbol"
}

enum class Literal(override val value: String) : TokenType {
    S64("s64"),
    F64("f64");

    override val category = "Literal"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
    override val isAssignment: Boolean = false
    override val isPostfix: Boolean = false
    override val isType: Boolean = false
}

object IDENTIFIER : TokenType {
    override val value: String? = "IDENTIFIER"
    override val category = "Identifier"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
    override val isAssignment: Boolean = false
    override val isPostfix: Boolean = false
    override val isType: Boolean = true

    override fun toString() = "IDENTIFIER"
}

object EOF : TokenType {
    override val value: String? = "EOF"
    override val category = "EOF"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
    override val isAssignment: Boolean = false
    override val isPostfix: Boolean = false
    override val isType: Boolean = false
    override fun toString() = "EOF"
}

data class Token(
    val row: Int,
    val col: Int,
    val pos: Int,
    val type: TokenType,
    val value: String
) {
    fun prettyPrint() = "%5d (%3d, %3d) %10s %-17s : %s".format(pos, row, col, type.category, type, value)
}

fun isKeyword(value: String) = Keyword.values().map { it.value }.contains(value)
fun isSymbol(value: String) = Symbol.values().map { it.value }.contains(value)
fun isIdentifier(value: String) = "^([a-z]|[A-Z]|_|\$)([a-z]|[A-Z]|[0-9]|_|\$)*$".toRegex().matches(value)
fun isLiteralS64(value: String) = "^([0-9])+$".toRegex().matches(value)
fun isLiteralF64(value: String) = "^([0-9])*\\.([0-9])+$".toRegex().matches(value)

class Lexer {
    fun lex(fileName: String): Queue<Token> {
        val fileContent = ArrayDeque(File(fileName).readText().toCharArray().toList())
        val tokens = ArrayDeque<Token>()

        var token: Token? = null
        var row = 0
        var col = 0

        var pos = 0
        while (fileContent.isNotEmpty()) {
            val char = fileContent.poll()

            when (char) {
                '\n' -> {
                    row++
                    col = 0
                    if (token != null) {
                        tokens.add(token)
                        token = null
                    }
                }
            }
            if (token != null) {
                val tempToken = "${token.value}$char"
                token = when {
                    isLiteralF64(tempToken) || (fileContent.isNotEmpty() && isLiteralF64(tempToken + fileContent.peek())) -> Token(
                        token.row,
                        token.col,
                        token.pos,
                        Literal.F64,
                        tempToken
                    )
                    isLiteralS64(tempToken) -> Token(token.row, token.col, token.pos, Literal.S64, tempToken)
                    isSymbol(tempToken) -> Token(
                        token.row,
                        token.col,
                        token.pos,
                        Symbol.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isKeyword(tempToken) -> Token(
                        token.row,
                        token.col,
                        token.pos,
                        Keyword.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isIdentifier(tempToken) -> Token(token.row, token.col, token.pos, IDENTIFIER, tempToken)
                    else -> {
                        tokens.add(token)
                        null
                    }
                }
            }
            if (token == null) {
                val tempToken = "$char"
                token = when {
                    isSymbol(tempToken) -> Token(
                        row,
                        col,
                        pos,
                        Symbol.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isKeyword(tempToken) -> Token(
                        row,
                        col,
                        pos,
                        Keyword.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isIdentifier(tempToken) -> Token(row, col, pos, IDENTIFIER, tempToken)
                    isLiteralS64(tempToken) -> Token(row, col, pos, Literal.S64, tempToken)
                    isLiteralF64(tempToken) -> Token(row, col, pos, Literal.F64, tempToken)
                    tempToken == "\n" -> {
                        row++
                        col = -1
                        null
                    }
                    else -> null
                }
            }

            pos++
            col++
        }

        if (token != null) {
            tokens.add(token)
        }

        tokens.add(Token(row, col, pos, EOF, "EOF"))
        return tokens
    }
}