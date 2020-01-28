package hps

import java.io.File
import java.util.*

interface TokenType {
    val value: String
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
    SWITCH("switch"),
    CASE("case"),
    CONST("const"),
    DEFAULT("default"),
    SIGNED("signed"),
    UNSIGNED("unsigned"),
    VOID("void", true),
    SHORT("short", true),
    INT("int", true),
    LONG("long", true),
    FLOAT("float", true),
    DOUBLE("double", true),
    CHAR("char", true),
    STRUCT("struct"),
    UNION("union"),
    ENUM("enum"),
    EXTERNAL("external"),
    TYPEDEF("typedef");

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
    AMPERSAND("&", 8, isBinary = true, isUnary = true),
    BAR("|", 10, isBinary = true),
    UP_TICK("^", 9, isBinary = true),
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
    DECREMENT("--", isUnary = true, isPostfix = true),
    SIZEOF("sizeof", isUnary = true);

    override val category = "Symbol"
}

enum class Literal(override val value: String) : TokenType {
    INT("int"),
    UNSIGNED_INT("unsigned int"),
    LONG("long"),
    UNSIGNED_LONG("unsigned long"),
    LONG_LONG("long long"),
    UNSIGNED_LONG_LONG("unsigned long long"),
    FLOAT("float"),
    DOUBLE("double"),
    CHAR("char"),
    STRING("char[]");

    override val category = "Literal"
    override val isBinary = false
    override val isUnary = false
    override val binaryPriority = -1
    override val isAssignment: Boolean = false
    override val isPostfix: Boolean = false
    override val isType: Boolean = false
}

object IDENTIFIER : TokenType {
    override val value: String = "IDENTIFIER"
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
    override val value: String = "EOF"
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
    fun prettyPrint() = "%5d (%3d, %3d) %10s %-21s : %s".format(pos, row, col, type.category, type, value)
}

fun isKeyword(value: String) = Keyword.values().map { it.value }.contains(value)
fun isSymbol(value: String) = Symbol.values().map { it.value }.contains(value)
fun isIdentifier(value: String) = "^([a-z]|[A-Z]|_|\$)([a-zA-Z0-9]|_|\$)*$".toRegex().matches(value)
fun isLiteralInt(value: String) = "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))$".toRegex().matches(value)
fun isLiteralUnsignedInt(value: String) =
    "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))+([Uu])$".toRegex().matches(value)

fun isLiteralLong(value: String) =
    "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))+([Ll])$".toRegex().matches(value)

fun isLiteralUnsignedLong(value: String) =
    "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))+([Uu][Ll])$".toRegex().matches(value)

fun isLiteralLongLong(value: String) =
    "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))+([Ll])([Ll])$".toRegex().matches(value)

fun isLiteralUnsignedLongLong(value: String) =
    "^([0-9]+|(0[bB](0-1)+)|(0([0-7])+)|(0[xX]([0-9a-fA-F])+))+([Uu][Ll])([Ll])$".toRegex().matches(value)

fun isLiteralFloat(value: String) = "^([0-9])+\\.([0-9])+([Ff])$".toRegex().matches(value)
fun isLiteralDouble(value: String) = "^([0-9])+\\.([0-9])+[Dd]?$".toRegex().matches(value)

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
                    if (token != null) {
                        tokens.add(token)
                        token = null
                    }
                }
            }
            if (token != null) {
                val tempToken = "${token.value}$char"
                token = when {
                    isLiteralFloat(tempToken) || (fileContent.isNotEmpty() && isLiteralFloat(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.FLOAT, tempToken)
                    isLiteralDouble(tempToken) || (fileContent.isNotEmpty() && isLiteralDouble(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.DOUBLE, tempToken)
                    isLiteralUnsignedLongLong(tempToken) || (fileContent.isNotEmpty() && isLiteralUnsignedLongLong(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.UNSIGNED_LONG_LONG, tempToken)
                    isLiteralLongLong(tempToken) || (fileContent.isNotEmpty() && isLiteralLongLong(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.LONG_LONG, tempToken)
                    isLiteralUnsignedLong(tempToken) || (fileContent.isNotEmpty() && isLiteralUnsignedLong(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.UNSIGNED_LONG, tempToken)
                    isLiteralLong(tempToken) || (fileContent.isNotEmpty() && isLiteralLong(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.LONG, tempToken)
                    isLiteralUnsignedInt(tempToken) || (fileContent.isNotEmpty() && isLiteralUnsignedInt(
                        tempToken + fileContent.peek()
                    )) -> Token(token.row, token.col, token.pos, Literal.UNSIGNED_INT, tempToken)
                    isLiteralInt(tempToken) -> Token(
                        token.row, token.col, token.pos, Literal.INT, tempToken
                    )
                    isSymbol(tempToken) -> Token(
                        token.row, token.col, token.pos,
                        Symbol.values().find { tempToken == it.value }!!, tempToken
                    )
                    isKeyword(tempToken) -> Token(
                        token.row, token.col, token.pos,
                        Keyword.values().find { tempToken == it.value }!!, tempToken
                    )
                    isIdentifier(tempToken) -> Token(token.row, token.col, token.pos, IDENTIFIER, tempToken)
                    tempToken.replace("\\'", "").startsWith("'") && tempToken.replace(
                        "\\'",
                        "'"
                    ).count { it == '\'' } == 1 -> Token(
                        token.row,
                        token.col,
                        token.pos,
                        Literal.CHAR,
                        tempToken
                    )
                    tempToken.replace("\\'", "").startsWith("'") && tempToken.replace("\\'", "").endsWith("'") -> {
                        Token(token.row, token.col, token.pos, Literal.CHAR, tempToken)
                    }
                    tempToken.replace("\\\"", "").startsWith("\"") && tempToken.replace(
                        "\\\"",
                        ""
                    ).count { it == '\"' } == 1 -> Token(
                        token.row,
                        token.col,
                        token.pos,
                        Literal.STRING,
                        tempToken
                    )
                    tempToken.replace("\\\"", "").startsWith("\"") && tempToken.replace("\\\"", "").endsWith("\"") -> {
                        Token(token.row, token.col, token.pos, Literal.STRING, tempToken)
                    }
                    else -> {
                        if (token.type == Literal.STRING || token.type == Literal.CHAR) {
                            token = Token(
                                token.row, token.col, token.pos, token.type, token.value
                                    .substring(1, token.value.length - 1)
//                                    .replace("\\b", "\b")
//                                    .replace("\\n", "\n")
//                                    .replace("\\r", "\r")
//                                    .replace("\\t", "\t")
//                                    .replace("\\\\", "\\")
//                                    .replace("\\'", "'")
//                                    .replace("\\\"", "\"")
                            )
                        }
                        tokens.add(token)
                        null
                    }
                }
            }
            if (token == null) {
                val tempToken = "$char"
                token = when {
                    isSymbol(tempToken) -> Token(
                        row, col, pos,
                        Symbol.values().find { tempToken == it.value }!!, tempToken
                    )
                    isKeyword(tempToken) -> Token(
                        row, col, pos,
                        Keyword.values().find { tempToken == it.value }!!, tempToken
                    )
                    isIdentifier(tempToken) -> Token(row, col, pos, IDENTIFIER, tempToken)
                    isLiteralInt(tempToken) -> Token(row, col, pos, Literal.INT, tempToken)
                    isLiteralFloat(tempToken) -> Token(row, col, pos, Literal.FLOAT, tempToken)
                    tempToken == "'" -> Token(row, col, pos, Literal.CHAR, tempToken)
                    tempToken == "\"" -> Token(row, col, pos, Literal.STRING, tempToken)
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
            if (token.type == Literal.STRING || token.type == Literal.CHAR) {
                token = Token(
                    token.row, token.col, token.pos, token.type, token.value
                        .substring(1, token.value.length - 1)
//                        .replace("\\b", "\b")
//                        .replace("\\n", "\n")
//                        .replace("\\r", "\r")
//                        .replace("\\t", "\t")
//                        .replace("\\\\", "\\")
//                        .replace("\\'", "'")
//                        .replace("\\\"", "\"")
                )
            }
            tokens.add(token)
        }

        tokens.add(Token(row, col, pos, EOF, "EOF"))
        return tokens
    }
}