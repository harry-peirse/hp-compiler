package hps

sealed class HPCode {
    abstract fun prettyPrint(): String

    data class Program(val structs: List<Struct>, val functions: List<Func>) : HPCode() {
        override fun prettyPrint() =
            structs.joinToString("\n") { it.prettyPrint() } + "\n" + functions.joinToString("\n") { it.prettyPrint() }
    }

    data class Argument(val type: Token, val name: Token, val isArray: Boolean) : HPCode() {
        override fun prettyPrint(): String = "${type.value}${if (isArray) "*" else ""} ${name.value}"
    }

    data class Struct(val name: Token, val arguments: List<Argument>) : HPCode() {
        override fun prettyPrint() =
            "struct ${name.value}_\n{\n${arguments.joinToString("\n") { "\t${it.prettyPrint()};" }}\n};"
    }

    sealed class Func : HPCode() {

        abstract val type: Token
        abstract val isArray: Boolean
        abstract val name: Token
        abstract val arguments: List<Argument>

        data class Implementation(
            override val type: Token,
            override val isArray: Boolean,
            override val name: Token,
            override val arguments: List<Argument>,
            val blockItems: List<BlockItem>
        ) : Func() {

            override fun prettyPrint() =
                "${type.value}${if (isArray) "*" else ""} ${name.value}(${arguments.joinToString(", ") { it.prettyPrint() }})\n{\n${blockItems.joinToString(
                    "\n"
                ) { it.prettyPrint() }}\n}\n"
        }

        data class External(
            override val type: Token,
            override val isArray: Boolean,
            override val name: Token,
            override val arguments: List<Argument>
        ) : Func() {
            override fun prettyPrint() = ""
        }
    }

    sealed class BlockItem : HPCode() {
        override fun prettyPrint() = prettyPrint(1)
        abstract fun prettyPrint(depth: Int, skipInitialTab: Boolean = false): String
        abstract fun blockItems(): List<BlockItem>
        abstract fun expressions(): List<Expression>

        sealed class Statement : BlockItem() {
            data class Return(val expression: Expression) : Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}return ${expression.prettyPrint()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
            }

            data class ProxyExpression(val expression: Expression) : Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}${expression.prettyPrint()};"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = listOf(expression)
            }

            data class Conditional(
                val condition: Expression,
                val statement: Statement,
                val elseStatement: Statement?
            ) :
                Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}if(${condition.prettyPrint()})\n${statement.prettyPrint(
                        depth + 1
                    )}${if (elseStatement != null) "\n${"\t".repeat(
                        depth
                    )}else${if (elseStatement is Compound) "\n${elseStatement.prettyPrint(depth + 1)}" else " ${elseStatement.prettyPrint(
                        depth,
                        true
                    )}"}" else ""}"

                override fun blockItems() = listOfNotNull<BlockItem>(statement, elseStatement)
                override fun expressions() = listOf(condition)
            }

            data class Compound(val blockItems: List<BlockItem>) : Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}{\n${blockItems.joinToString("\n") {
                        it.prettyPrint(
                            depth + 1
                        )
                    }}\n${"\t".repeat(
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
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}for(${initialization.prettyPrint()}; ${if (condition is Expression.Empty) "1" else condition.prettyPrint()}; ${increment.prettyPrint()}) \n${statement.prettyPrint(
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
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}for(${declaration.prettyPrint(0)} ${if (condition is Expression.Empty) "1" else condition.prettyPrint()}; ${increment.prettyPrint()}) \n${statement.prettyPrint(
                        depth
                    )}"

                override fun blockItems() = listOf(declaration, statement)
                override fun expressions() = listOf(condition, increment)
            }

            data class While(val condition: Expression, val statement: Statement) :
                Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}while(${condition.prettyPrint()}) \n${statement.prettyPrint(
                        depth + 1
                    )}"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            data class DoWhile(val statement: Statement, val condition: Expression) :
                Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${"\t".repeat(depth)}do \n${statement.prettyPrint(depth + 1)} \n${if (skipInitialTab) "" else "\t".repeat(
                        depth
                    )}while (${condition.prettyPrint()});"

                override fun blockItems() = listOf<BlockItem>(statement)
                override fun expressions() = listOf(condition)
            }

            object Break : Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}break;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }

            object Continue : Statement() {
                override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                    "${if (skipInitialTab) "" else "\t".repeat(depth)}continue;"

                override fun blockItems() = emptyList<BlockItem>()
                override fun expressions() = emptyList<Expression>()
            }
        }

        data class Declaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value} = ${expression?.prettyPrint()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }

        data class ArrayDeclaration(
            val type: Token,
            val name: Token,
            val expression: Expression?
        ) : BlockItem() {
            override fun prettyPrint(depth: Int, skipInitialTab: Boolean) =
                "${if (skipInitialTab) "" else "\t".repeat(depth)}${type.value} ${name.value}[] = ${expression?.prettyPrint()
                    ?: ""};"

            override fun blockItems() = emptyList<BlockItem>()
            override fun expressions() = listOfNotNull(expression)
        }
    }

    sealed class Expression : HPCode() {

        abstract fun flattened(): List<Expression>

        data class Constant(val value: Token) : Expression() {
            override fun prettyPrint() = when (value.type) {
                Literal.CHAR -> "'${value.value}'"
                Literal.STRING -> "\"${value.value}\""
                else -> value.value
            }

            override fun flattened() = listOf(this)
        }

        data class ArrayConstant(val values: List<Expression>, var type: String) :
            Expression() {
            override fun prettyPrint() = "($type[]){${values.joinToString(", ") { it.prettyPrint() }}}"
            override fun flattened() = values.flatMap { it.flattened() } + this
        }

        data class Unary(val unaryOp: Token, val expression: Expression, val postfix: Boolean) : Expression() {
            override fun prettyPrint() =
                if (postfix) "(${expression.prettyPrint()}${unaryOp.value})" else "(${unaryOp.value}${expression.prettyPrint()})"

            override fun flattened() = expression.flattened() + this
        }

        data class Binary(
            val binaryOp: Token,
            val firstExpression: Expression,
            val secondExpression: Expression
        ) :
            Expression() {
            override fun prettyPrint() = "(${firstExpression.prettyPrint()}${binaryOp.value
                .replace("and", "&&")
                .replace("or", "||")}${secondExpression.prettyPrint()})"

            override fun flattened() = firstExpression.flattened() + secondExpression.flattened() + this
        }

        data class Nested(val expression: Expression) : Expression() {
            override fun prettyPrint() = expression.prettyPrint()
            override fun flattened() = expression.flattened() + this
        }

        data class Assign(
            val name: Token,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun prettyPrint() = "${name.value} ${assignment.value} ${expression.prettyPrint()}"
            override fun flattened() = expression.flattened() + this
        }

        data class AssignArrayIndex(
            val name: Token,
            val index: Expression,
            val assignment: Token,
            val expression: Expression
        ) :
            Expression() {
            override fun prettyPrint() =
                "${name.value}[${index.prettyPrint()}] ${assignment.value} ${expression.prettyPrint()}"

            override fun flattened() = index.flattened() + expression.flattened() + this
        }

        data class Variable(val name: Token) : Expression() {
            override fun prettyPrint() = name.value
            override fun flattened() = listOf(this)
        }

        data class ArrayVariable(val name: Token, val index: Expression) : Expression() {
            override fun prettyPrint() = "${name.value}[${index.prettyPrint()}]"
            override fun flattened() = index.flattened() + this
        }

        data class Conditional(
            val condition: Expression,
            val expression: Expression,
            val elseExpression: Expression
        ) :
            Expression() {
            override fun prettyPrint() =
                "${condition.prettyPrint()} ? ${expression.prettyPrint()} : ${elseExpression.prettyPrint()}"

            override fun flattened() =
                condition.flattened() + expression.flattened() + elseExpression.flattened() + this
        }

        object Empty : Expression() {
            override fun prettyPrint() = ""
            override fun flattened() = listOf(this)
        }

        data class FunctionCall(val name: Token, val arguments: List<Expression>, var isConstructor: Boolean = false) :
            Expression() {
            override fun prettyPrint() =
                if (isConstructor) "(${name.value}){${arguments.joinToString(", ") { it.prettyPrint() }}}"
                else "${name.value}(${arguments.joinToString(", ") { it.prettyPrint() }})"

            override fun flattened() = arguments.flatMap { it.flattened() } + this
        }
    }
}