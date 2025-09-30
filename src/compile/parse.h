#ifndef __PARSE_H__
#define __PARSE_H__

#include <types.h>
#include <compilerState.h>
#include "tokenize.h"


typedef uint32 Expression;
enum ExpressionType{
	EXPRESSION_TYPE_NONE,
	EXPRESSION_TYPE_UNARY,
	EXPRESSION_TYPE_BINARY,
	
	EXPRESSION_TYPE_PRIMARY,
	EXPRESSION_TYPE_EXPRESSION,
	EXPRESSION_TYPE_NUMBER_LITERAL,
	EXPRESSION_TYPE_STRING_LITERAL,
	EXPRESSION_TYPE_IDENTIFIER,
};

enum UnaryType {
	UNARY_TYPE_NEGATE,
	UNARY_TYPE_LOGICAL_NOT,
};

struct UnaryExpression {
	enum UnaryType type;
};

enum BinaryType {
	BINARY_TYPE_ADDITION,
	BINARY_TYPE_SUBTRACTION,
	BINARY_TYPE_MULTIPLICATION,
	BINARY_TYPE_DIVISION,
	BINARY_TYPE_MODULO,
};

struct NumericExpression {
	Token* number;
};

struct BinaryExpression {
	Expression left;
	enum BinaryType type;
	Expression right;
};

struct Expression{
	const enum ExpressionType type;
	const uint64 expressionStart;
	const uint64 expressionEnd;
	union {
		const Expression expression;
		const struct UnaryExpression unary;
		const struct BinaryExpression binary;
		const Token* numeric;
		const Token* string;
		const Token* identifier;
	};
};



struct AbstractSyntaxTree {
	void* astMemory;
};

bool32 parse(struct CompilerCommand command, struct TokenList input, struct AbstractSyntaxTree* ast);

void printAST(struct TokenList tokens, struct AbstractSyntaxTree* ast);

#endif//__PARSE_H__
