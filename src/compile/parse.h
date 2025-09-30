#ifndef __PARSE_H__
#define __PARSE_H__

#include <types.h>
#include <compilerState.h>
#include "tokenize.h"


enum ExpressionType{
	EXPRESSION_TYPE_NONE,
	EXPRESSION_TYPE_UNARY,
	EXPRESSION_TYPE_FACTOR,
	EXPRESSION_TYPE_TERM,
	EXPRESSION_TYPE_COMPARISON,
	EXPRESSION_TYPE_EQUALITY,
	
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

struct BinaryExpression {
	struct Expression* expressionLeft;
	enum BinaryType type;
	struct Expression* expressionRight;
};

typedef struct Expression{
	enum ExpressionType type;
	uint64 tokenStart;
	uint64 tokenEnd;
	union {
		struct Expression* expression;
		struct UnaryExpression unary;
		struct BinaryExpression binary;
	};
} Expression;


struct AbstractSyntaxTree {

};

bool32 parse(struct CompilerCommand command, struct TokenList input, struct AbstractSyntaxTree* tokens);

void printAST(struct TokenList tokens);

#endif//__PARSE_H__
