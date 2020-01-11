package com.aal.hp

import java.io.File
import java.util.*

interface TokenType {
    val value: String?
    val category: String
    val isUnary: Boolean
    val isBinary: Boolean
    val binaryPriority: Int
}

enum class Keyword(override val value: String) : TokenType {
    RETURN("return"),
    INT("int");

    override val category = "Keyword"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
}

enum class Symbol(
    override val value: String,
    override val binaryPriority: Int = -1,
    override val isUnary: Boolean = false,
    override val isBinary: Boolean = false
) : TokenType {
    OPEN_BRACKETS("("),
    CLOSE_BRACKETS(")"),
    OPEN_BRACE("{"),
    CLOSE_BRACE("}"),
    OPEN_ARRAY("["),
    CLOSE_ARRAY("]"),
    SEMICOLON(";"),
    PERIOD("."),
    COMMA(","),
    ARROW("->"),
    QUESTION_MARK("?"),
    COLON(":"),
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
    ASSIGN("=", 14, isBinary = true),
    AND("&&", 11, isBinary = true),
    OR("||", 12, isBinary = true),
    EQUAL("==", 7, isBinary = true),
    NOT_EQUAL("!=", 7, isBinary = true),
    LESS_THAN("<", 6, isBinary = true),
    LESS_THAN_OR_EQUAL_TO("<=", 6, isBinary = true),
    GREATER_THAN(">", 6, isBinary = true),
    GREATER_THAN_OR_EQUAL_TO(">=", 6, isBinary = true),
    PLUS_ASSIGN("+=", 14, isBinary = true),
    MINUS_ASSIGN("-=", 14, isBinary = true),
    TIMES_ASSIGN("*=", 14, isBinary = true),
    DIVIDE_ASSIGN("/=", 14, isBinary = true),
    MODULUS_ASSIGN("%=", 14, isBinary = true),
    BITWISE_SHIFT_LEFT_ASSIGN("<<=", 14, isBinary = true),
    BITWISE_SHIFT_RIGHT_ASSIGN(">>=", 14, isBinary = true),
    BITWISE_AND_ASSIGN("&=", 14, isBinary = true),
    BITWISE_OR_ASSIGN("|=", 14, isBinary = true),
    BITWISE_XOR_ASSIGN("^=", 14, isBinary = true),
    INCREMENT("++", isUnary = true),
    DECREMENT("--", isUnary = true);

    override val category = "Symbol"
}

enum class Literal() : TokenType {
    INT;

    override val value: String? = null
    override val category = "Literal"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
}

object IDENTIFIER : TokenType {
    override val value: String? = null
    override val category = "Identifier"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1

    override fun toString() = "IDENTIFIER"
}

data class Token(
    val row: Int,
    val col: Int,
    val pos: Int,
    val type: TokenType,
    val value: String
) {
    override fun toString() = "%5d (%3d, %3d) %10s %-17s : %s".format(pos, row, col, type.category, type, value)
}

object Lexer {
    private fun isKeyword(value: String) = Keyword.values().map { it.value }.contains(value)
    private fun isSymbol(value: String) = Symbol.values().map { it.value }.contains(value)
    private fun isIdentifier(value: String) = "^([a-z]|[A-Z]|_|\$)([a-z]|[A-Z]|[0-9]|_|\$)*$".toRegex().matches(value)
    private fun isLiteralInt(value: String) = "^([0-9])+$".toRegex().matches(value)

    fun lex(fileName: String): Queue<Token> {
        val fileContent = File(fileName).readText()
        val tokens = ArrayDeque<Token>()

        var token: Token? = null
        var row = 0
        var col = 0

        for (i in fileContent.indices) {
            val char = fileContent[i]

            when (char) {
                '\n' -> {
                    row++
                    col = 0
                    token = null
                }
            }
            if (token != null) {
                val tempToken = "${token.value}$char"
                token = when {
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
                    isLiteralInt(tempToken) -> Token(token.row, token.col, token.pos, Literal.INT, tempToken)
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
                        i,
                        Symbol.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isKeyword(tempToken) -> Token(
                        row,
                        col,
                        i,
                        Keyword.values().find { tempToken == it.value }!!,
                        tempToken
                    )
                    isIdentifier(tempToken) -> Token(row, col, i, IDENTIFIER, tempToken)
                    isLiteralInt(tempToken) -> Token(row, col, i, Literal.INT, tempToken)
                    tempToken == "\n" -> {
                        row++
                        col = -1
                        null
                    }
                    else -> null
                }
            }

            col++
        }

        if (token != null) {
            tokens.add(token)
        }

        return tokens
    }
}