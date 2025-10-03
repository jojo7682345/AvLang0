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
	EXPRESSION_TYPE_FUNCTION_CALL,
	EXPRESSION_TYPE_ASSIGNMENT,
	EXPRESSION_TYPE_NUMBER_LITERAL,
	EXPRESSION_TYPE_STRING_LITERAL,
	EXPRESSION_TYPE_IDENTIFIER,
};

enum UnaryType {
	UNARY_TYPE_NEGATE,
	UNARY_TYPE_LOGICAL_NOT,
	UNARY_TYPE_PRE_INCREMENT,
	UNARY_TYPE_PRE_DECREMENT,
	UNARY_TYPE_POST_INCREMENT,
	UNARY_TYPE_POST_DECREMENT,
	UNARY_TYPE_LOGICAL_INVERT,
};

struct UnaryExpression {
	enum UnaryType type;
	Expression inner;
};

enum BinaryType {
	BINARY_TYPE_ADDITION,
	BINARY_TYPE_SUBTRACTION,
	BINARY_TYPE_MULTIPLICATION,
	BINARY_TYPE_DIVISION,
	BINARY_TYPE_MODULO,
	BINARY_TYPE_LESS_THAN,
	BINARY_TYPE_LESS_THAN_OR_EQUAL,
	BINARY_TYPE_GREATER_THAN,
	BINARY_TYPE_GREATER_THAN_OR_EQUAL,
	BINARY_TYPE_EQUALS,
	BINARY_TYPE_NOT_EQUALS,
};

struct BinaryExpression {
	Expression left;
	enum BinaryType type;
	Expression right;
};

struct FunctionCallExpression {
	Token* identifier;
	Expression* arguments;
	uint32 argumentCount;
};

enum AssignmentType {
	ASSIGNMENT_TYPE_DIRECT,
	ASSIGNMENT_TYPE_INCREMENT,
	ASSIGNMENT_TYPE_DECREMENT,
	ASSIGNMENT_TYPE_MULTIPLY,
	ASSIGNMENT_TYPE_DIVIDE,
};

struct AssignmentExpression {
	Token* identifier;
	enum AssignmentType type;
	Expression value;
};

struct Expression{
	const enum ExpressionType type;
	const uint64 expressionStart;
	const uint64 expressionEnd;
	union {
		const Expression expression;
		const struct UnaryExpression unary;
		const struct BinaryExpression binary;
		const struct FunctionCallExpression functionCall;
		const struct AssignmentExpression assignment;
		const Token* numeric;
		const Token* string;
		const Token* identifier;
	};
};

typedef uint32 Statement;
typedef uint32 Type;
typedef uint32 Tag;

struct Tag {
	Token* identifier;
	Expression* values;
	uint32 valueCount;
};

struct Type {
	Token* identifier;
	Tag* tags;
	uint32 tagCount;
};


enum StatementType {
	STATEMENT_TYPE_EXPRESSION,
	STATEMENT_TYPE_BLOCK,
	STATEMENT_TYPE_DECLARATION,
	STATEMENT_TYPE_IF,
	STATEMENT_TYPE_RETURN,
	STATEMENT_TYPE_TYPEDEF,
	STATEMENT_TYPE_FUNCTION,
};

struct BlockStatement {
	Statement* statements;
	uint32 statementCount;
};

struct IfStatement {
	Expression value;
	Statement block;
	Statement elseBlock;
};

struct TypedefStatement {
	Type type;
	Token* identifier;
};

struct DeclarationStatement {
	Type type;
	Token* identifier;
	Expression initializer;
};

struct FunctionParameter {
	Type type;
	Token* identifier;
};

struct FunctionStatement {
	Type returnType;
	Token* identifier;
	Tag* tags;
	uint32 tagCount;
	struct FunctionParameter* parameters;
	uint32 parameterCount;
	Statement body;
};

struct Statement{
	const enum StatementType type;
	const uint64 statementStart;
	const uint64 statementEnd;
	union {
		const Expression expression;
		const struct BlockStatement block;
		const struct IfStatement branch;
		const struct TypedefStatement typeDefine;
		const struct DeclarationStatement declaration;
		const struct FunctionStatement function;
	};
};



struct AbstractSyntaxTree {
	struct Expression* expressions;
	uint32 rootExpression;
};

bool32 parse(struct CompilerCommand command, struct TokenList input, struct AbstractSyntaxTree* ast);

void printAST(struct TokenList tokens, struct AbstractSyntaxTree* ast);

#endif//__PARSE_H__
