package hps.c

import hps.*
import hps.c.CCode.*
import hps.c.CCode.Function
import java.util.*

sealed class CCode {
    abstract fun toC(): String

    data class Program(val structs: List<Struct>, val functions: List<Function>) : CCode() {
        override fun toC() = structs.joinToString("\n") { it.toC() } + "\n" + functions.joinToString("\n") { it.toC() }
    }

    data class Argument(val type: String, val name: String) : CCode() {
        override fun toC(): String = "$type $name"
    }

    data class Struct(val name: String, val arguments: List<Argument>) : CCode() {
        override fun toC() =
            "typedef struct ${name}_\n{\n${arguments.joinToString("\n") { "\t${it.toC()};" }}\n} $name;"
    }

    data class Function(
        val type: String,
        val name: String,
        val arguments: List<Argument>,
        val blockItems: List<BlockItem>
    ) : CCode() {
        override fun toC() =
            "$type $name(${arguments.joinToString(", ") { it.toC() }})\n{\n${blockItems.joinToString("\n") { it.toC() }}\n}\n"
    }

    sealed class BlockItem : CCode() {
        override fun toC() = toC(1)
        abstract fun toC(depth: Int, skipInitialTab: Boolean = false): String
        abstract fun blockItems(): List<BlockItem>
        abstract fun expressions(): List<Expression>

        sealed class Statement : BlockItem() {
            data class Return(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}return ${expression.toC()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
            }

            data class ProxyExpression(val expression: Expression) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${expression.toC()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
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

                override fun blockItems() = listOfNotNull<BlockItem>(statement, elseStatement)
                override fun expressions() = listOf(condition)
            }

            data class Compound(val blockItems: List<BlockItem>) : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}{\n${blockItems.joinToString("\n") { it.toC(depth + 1) }}\n${"\t".repeat(
                        depth
                    )}}"

                override fun blockItems() = blockItems
                override fun expressions() = emptyList<Expression>()
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

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(initialization, condition, increment)
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

                override fun blockItems() = listOf(declaration, statement)
                override fun expressions() = listOf(condition, increment)
            }

            data class While(val condition: Expression, val statement: Statement) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}while(${condition.toC()}) \n${statement.toC(depth + 1)}"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            data class DoWhile(val statement: Statement, val condition: Expression) :
                Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${"\t".repeat(depth)}do \n${statement.toC(depth + 1)} \n${if (skipInitialTab) "" else "\t".repeat(
                        depth
                    )}while (${condition.toC()});"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            object Break : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}break;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }

            object Continue : Statement() {
                override fun toC(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}continue;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }
        }

        data class Declaration(
            val type: String,
            val name: String,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}$type $name = ${expression?.toC()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }

        data class ArrayDeclaration(
            val type: String,
            val name: String,
            val expression: Expression?
        ) : BlockItem() {
            override fun toC(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}$type $name[] = ${expression?.toC()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }
    }

    sealed class Expression : CCode() {

        abstract fun flattened(): List<Expression>

        data class Constant(val value: String) : Expression() {
            override fun toC() = value
            override fun flattened() = listOf(this)
        }

        data class ArrayConstant(val values: List<Expression>, var type: String) :
            Expression() {
            override fun toC() = "($type[]){${values.joinToString(", ") { it.toC() }}}"
            override fun flattened() = values.flatMap { it.flattened() } + this
        }

        data class Unary(val unaryOp: String, val expression: Expression, val postfix: Boolean) : Expression() {
            override fun toC() =
                if (postfix) "(${expression.toC()}$unaryOp)" else "($unaryOp${expression.toC()})"

            override fun flattened() = expression.flattened() + this
        }

        data class Binary(
            val binaryOp: String,
            val firstExpression: Expression,
            val secondExpression: Expression
        ) :
            Expression() {
            override fun toC() = "(${firstExpression.toC()}$binaryOp${secondExpression.toC()})"

            override fun flattened() = firstExpression.flattened() + secondExpression.flattened() + this
        }

        data class Nested(val expression: Expression) : Expression() {
            override fun toC() = expression.toC()
            override fun flattened() = expression.flattened() + this
        }

        data class Assign(
            val name: String,
            val assignment: String,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "$name $assignment ${expression.toC()}"
            override fun flattened() = expression.flattened() + this
        }

        data class AssignArrayIndex(
            val name: String,
            val index: Expression,
            val assignment: String,
            val expression: Expression
        ) :
            Expression() {
            override fun toC() = "$name[${index.toC()}] $assignment ${expression.toC()}"
            override fun flattened() = index.flattened() + expression.flattened() + this
        }

        data class Variable(val name: String) : Expression() {
            override fun toC() = name
            override fun flattened() = listOf(this)
        }

        data class ArrayVariable(val name: String, val index: Expression) : Expression() {
            override fun toC() = "$name[${index.toC()}]"
            override fun flattened() = index.flattened() + this
        }

        data class Conditional(
            val condition: Expression,
            val expression: Expression,
            val elseExpression: Expression
        ) :
            Expression() {
            override fun toC() = "${condition.toC()} ? ${expression.toC()} : ${elseExpression.toC()}"
            override fun flattened() =
                condition.flattened() + expression.flattened() + elseExpression.flattened() + this
        }

        object Empty : Expression() {
            override fun toC() = ""
            override fun flattened() = listOf(this)
        }

        data class FunctionCall(val name: String, val arguments: List<Expression>, var isConstructor: Boolean = false) :
            Expression() {
            override fun toC() =
                if (isConstructor) "($name){${arguments.joinToString(", ") { it.toC() }}}"
                else "$name(${arguments.joinToString(", ") { it.toC() }})"

            override fun flattened() = arguments.flatMap { it.flattened() } + this
        }
    }
}

fun parseProgram(hpProgram: HPCode.Program): CCode.Program {
    TODO()
}