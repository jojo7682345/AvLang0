#include "../tokenize.h"
#include "types.h"
#include "../parse.h"
#include <utils.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <AvUtils/avTypes.h>
#define AV_DYNAMIC_ARRAY_ADVANCED
#define AV_DYNAMIC_ARRAY_EXPOSE_MEMORY_LAYOUT
#include <AvUtils/dataStructures/avDynamicArray.h>
#include <AvUtils/avMemory.h>
#define AV_LOG_CATEGORY PARSE_CATEGORY
#include <logging.h>


typedef struct AbstractSyntaxTree AbstractSyntaxTree;
typedef struct Parser {
	struct TokenList* tokens;
	AvDynamicArray expressions;
}Parser;



#define match(input, type, ...) _scan(input, true, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define check(input, type, ...) _scan(input, false, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define consume(input, type, ...) _scan(input, true, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define consumeUpTo(input, type, ...) while(!check(input,type,__VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)){input->tokens->currentToken += 1;}
static const struct Token* _scan(Parser* input, bool32 advance, enum TokenType type, ...){
	if(input->tokens->currentToken >= input->tokens->tokenCount){
		return nullptr;
	}
	const Token* token = &input->tokens->tokens[input->tokens->currentToken];
	if(token->type == type){
		input->tokens->currentToken += advance ? 1 : 0;
		return token;
	}
	va_list args;
	va_start(args, type);

	while((type = va_arg(args, enum TokenType))){
		if(token->type == type){
			va_end(args);
			input->tokens->currentToken += advance ? 1 : 0;
			return token;
		}
	}
	
	va_end(args);
	return nullptr;
}

#define logSyntaxError(parser, ...) _logSyntaxError(parser, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
void _logSyntaxError(Parser* parser,...){

}

uint64 getTokenStart(const Token* const token){
	return token->position.offset;
}

uint64 getTokenEnd(const Token* const token){
	return token->position.offset + token->text.length - 1;
}
#define GET_EXPRESSION(expression, parser) ((struct Expression*)avDynamicArrayGetPtr(expression, parser->expressions))
#define INVALID_EXPRESSION ((uint32)-1)
#define BINARY_EXPRESSION(parser, leftExpression, operator, rightExpression) {\
	.type=EXPRESSION_TYPE_BINARY,\
	.expressionStart = GET_EXPRESSION(leftExpression, parser)->expressionStart,\
	.expressionEnd = GET_EXPRESSION(rightExpression, parser)->expressionEnd,\
	.binary = {\
		.left = leftExpression,\
		.type = operator,\
		.right = rightExpression,\
	},\
}
#define CREATE_BINARY_EXPRESSION(parser, left, operator, right) \
	createExpression(parser, (struct Expression) BINARY_EXPRESSION(parser, left, operator, right))
static Expression createExpression(Parser* parser, struct Expression expression){
	return avDynamicArrayAdd(&expression, parser->expressions);
}

static Expression parseExpression(Parser* parser);

static Expression parsePrimary(Parser* parser){
	const Token* token;
	if((token = match(parser, TOKEN_TYPE_NUMERIC))){
		struct Expression expr = {
			.type = EXPRESSION_TYPE_NUMBER_LITERAL,
			.expressionStart = getTokenStart(token),
			.expressionEnd = getTokenEnd(token),
			.numeric = token,
		};
		return createExpression(parser, expr);
	}
	if((token = match(parser, TOKEN_TYPE_STRING))){
		struct Expression expr = {
			.type = EXPRESSION_TYPE_STRING_LITERAL,
			.expressionStart = getTokenStart(token),
			.expressionEnd = getTokenEnd(token),
			.string = token,
		};
		return createExpression(parser, expr);
	}
	if((token = match(parser, TOKEN_TYPE_IDENTIFIER))){
		struct Expression expr = {
			.type = EXPRESSION_TYPE_IDENTIFIER,
			.expressionStart = getTokenStart(token),
			.expressionEnd = getTokenEnd(token),
			.identifier = token,
		};
		return createExpression(parser, expr);
	}
	if((token = match(parser, TOKEN_TYPE_PARENTHESESE_OPEN))){
		uint64 expressionStart = getTokenStart(token);
		Expression inner = parseExpression(parser);
		if(inner == INVALID_EXPRESSION){
			consumeUpTo(parser, TOKEN_TYPE_PARENTHESESE_CLOSE);
			struct Expression expr = {
				.type = EXPRESSION_TYPE_NONE,
			};
			inner = createExpression(parser, expr);
		}
		token = consume(parser, TOKEN_TYPE_PARENTHESESE_CLOSE);
		if(!token){
			logSyntaxError(parser, TOKEN_TYPE_PARENTHESESE_CLOSE);
			return INVALID_EXPRESSION;
		}
		return inner;
	}
}

static Expression parseUnary(Parser* parser){
	const Token* token;
	if((token = match(parser, TOKEN_TYPE_MINUS, TOKEN_TYPE_LOGICAL_NOT, TOKEN_TYPE_INCREMENT, TOKEN_TYPE_DECREMENT, TOKEN_TYPE_LOGICAL_INVERT))){
		uint64 expressionStart = getTokenStart(token);
		enum UnaryType operator;
		switch(token->type){
			case TOKEN_TYPE_MINUS:
				operator = UNARY_TYPE_NEGATE;
				break;
			case TOKEN_TYPE_LOGICAL_NOT:
				operator = UNARY_TYPE_LOGICAL_NOT;
				break;
			case TOKEN_TYPE_INCREMENT:
				operator = UNARY_TYPE_PRE_INCREMENT;
				break;
			case TOKEN_TYPE_DECREMENT:
				operator = UNARY_TYPE_PRE_DECREMENT;
				break;
			case TOKEN_TYPE_LOGICAL_INVERT:
				operator = UNARY_TYPE_LOGICAL_INVERT;
				break;
			default:
				return INVALID_EXPRESSION;
		}
		Expression inner = parseUnary(parser);
		struct Expression expr = {
			.type = EXPRESSION_TYPE_UNARY,
			.expressionStart = expressionStart,
			.expressionEnd = GET_EXPRESSION(inner, parser)->expressionEnd,
			.unary = {
				.type = operator,
				.inner = inner,
			},
		};
		return createExpression(parser, expr);
	}
	
	Expression primary = parsePrimary(parser);

	if((token = match(parser, TOKEN_TYPE_INCREMENT, TOKEN_TYPE_DECREMENT))){
		enum UnaryType operator;
		switch(token->type){
			case TOKEN_TYPE_INCREMENT:
				operator = UNARY_TYPE_POST_INCREMENT;
				break;
			case TOKEN_TYPE_DECREMENT:
				operator = UNARY_TYPE_POST_DECREMENT;
				break;
			default: return INVALID_EXPRESSION;
		}
		struct Expression expr = {
			.type = EXPRESSION_TYPE_UNARY,
			.expressionStart = GET_EXPRESSION(primary, parser)->expressionStart,
			.expressionEnd = getTokenEnd(token),
			.unary = {
				.type = operator,
				.inner = primary,
			},
		};
		return createExpression(parser, expr);
	}

	return primary;
}

static Expression parseFactor(Parser* parser){
	Expression left = parseUnary(parser);

	const Token* token;
	while((token = match(parser, TOKEN_TYPE_MULTIPLY, TOKEN_TYPE_DIVIDE, TOKEN_TYPE_MODULUS))){
		enum BinaryType operator;
		switch(token->type){
			case TOKEN_TYPE_MULTIPLY:
				operator = BINARY_TYPE_MULTIPLICATION;
				break;
			case TOKEN_TYPE_DIVIDE:
				operator = BINARY_TYPE_DIVISION;
				break;
			case TOKEN_TYPE_MODULUS:
				operator = BINARY_TYPE_MODULO;
				break;
			default: return INVALID_EXPRESSION;
		}

		Expression right = parseUnary(parser);
		struct Expression expr = {
			.type = EXPRESSION_TYPE_BINARY,
			.expressionStart = GET_EXPRESSION(left, parser)->expressionStart,
			.expressionEnd = GET_EXPRESSION(right, parser)->expressionEnd,
			.binary = {
				.left = left,
				.type = operator,
				.right = right,
			},
		};
		left = createExpression(parser, expr);
	}

	return left;
}

static Expression parseTerm(Parser* parser){
	Expression left = parseFactor(parser);

	const Token* token;
	while((token = match(parser, TOKEN_TYPE_PLUS, TOKEN_TYPE_MINUS))){
		enum BinaryType operator;
		switch(token->type){
			case TOKEN_TYPE_PLUS:
				operator = BINARY_TYPE_ADDITION;
				break;
			case TOKEN_TYPE_MINUS:
				operator = BINARY_TYPE_SUBTRACTION;
				break;
			default: return INVALID_EXPRESSION;
		}

		Expression right = parseFactor(parser);
		struct Expression expr = {
			.type = EXPRESSION_TYPE_BINARY,
			.expressionStart = GET_EXPRESSION(left, parser)->expressionStart,
			.expressionEnd = GET_EXPRESSION(right, parser)->expressionEnd,
			.binary = {
				.left = left,
				.type = operator,
				.right = right,
			},
		};
		left = createExpression(parser, expr);
	}

	return left;
}

static Expression parseComparison(Parser* parser){
	Expression left = parseTerm(parser);

	const Token* token;
	while((token = match(parser, 
		TOKEN_TYPE_LESS_THAN, TOKEN_TYPE_LESS_THAN_OR_EQUALS, 
		TOKEN_TYPE_GREATER_THAN, TOKEN_TYPE_GREATER_THAN_OR_EQUALS,
		TOKEN_TYPE_EQUALS, TOKEN_TYPE_NOT_EQUALS
	))){
		enum BinaryType operator;
		switch(token->type){
			case TOKEN_TYPE_LESS_THAN:
				operator = BINARY_TYPE_LESS_THAN;
				break;
			case TOKEN_TYPE_LESS_THAN_OR_EQUALS:
				operator = BINARY_TYPE_LESS_THAN_OR_EQUAL;
				break;
			case TOKEN_TYPE_GREATER_THAN:
				operator = BINARY_TYPE_GREATER_THAN;
				break;
			case TOKEN_TYPE_GREATER_THAN_OR_EQUALS:
				operator = BINARY_TYPE_GREATER_THAN_OR_EQUAL;
				break;
			case TOKEN_TYPE_EQUALS:
				operator = BINARY_TYPE_EQUALS;
				break;
			case TOKEN_TYPE_NOT_EQUALS:
				operator = BINARY_TYPE_NOT_EQUALS;
				break;
			default: return INVALID_EXPRESSION;
		}

		Expression right = parseUnary(parser);
		struct Expression expr = {
			.type = EXPRESSION_TYPE_BINARY,
			.expressionStart = GET_EXPRESSION(left, parser)->expressionStart,
			.expressionEnd = GET_EXPRESSION(right, parser)->expressionEnd,
			.binary = {
				.left = left,
				.type = operator,
				.right = right, 
			},
		};
		left = createExpression(parser, expr);
	}

	return left;
}

static Expression parseExpression(Parser* parser){
	return parseComparison(parser);
}

bool32 parse(struct CompilerCommand command, struct TokenList input, AbstractSyntaxTree* ast){	

	Parser parser = {
		.tokens = &input,
	};
	avDynamicArrayCreate(0, sizeof(struct Expression), &parser.expressions);
	
	ast->rootExpression = parseExpression(&parser);
	ast->expressions = avCallocate(avDynamicArrayGetSize(parser.expressions), sizeof(struct Expression), "");
	avDynamicArrayReadRange(ast->expressions, avDynamicArrayGetSize(parser.expressions), 0, sizeof(struct Expression), 0, parser.expressions);
	return true;
}
// Helper for indentation
static void printIndent(int level) {
    for (int i = 0; i < level; ++i) {
        printf("  ");
    }
}

// Recursive expression printer
static void printExpression(struct Expression* expressions, Expression index, int indentLevel) {
    struct Expression* expr = expressions + index;
    if (!expr) {
        printIndent(indentLevel);
        printf("<invalid expression>\n");
        return;
    }

    printIndent(indentLevel);
    switch (expr->type) {
        case EXPRESSION_TYPE_NUMBER_LITERAL:
            printf("NumberLiteral: %.*s\n",
                (int)expr->numeric->text.length,
                expr->numeric->text.start);
            break;

        case EXPRESSION_TYPE_STRING_LITERAL:
            printf("StringLiteral: \"%.*s\"\n",
                (int)expr->string->text.length,
                expr->string->text.start);
            break;

        case EXPRESSION_TYPE_IDENTIFIER:
            printf("Identifier: %.*s\n",
                (int)expr->identifier->text.length,
                expr->identifier->text.start);
            break;

        case EXPRESSION_TYPE_UNARY:
            printf("UnaryExpression (%d):\n", expr->unary.type);
            printExpression(expressions, expr->unary.inner, indentLevel + 1);
            break;

        case EXPRESSION_TYPE_BINARY:
            printf("BinaryExpression (%d):\n", expr->binary.type);
            printIndent(indentLevel + 1);
            printf("Left:\n");
            printExpression(expressions, expr->binary.left, indentLevel + 2);
            printIndent(indentLevel + 1);
            printf("Right:\n");
            printExpression(expressions, expr->binary.right, indentLevel + 2);
            break;

        case EXPRESSION_TYPE_NONE:
            printf("Invalid/Empty Expression\n");
            break;

        default:
            printf("Unknown Expression Type (%d)\n", expr->type);
            break;
    }
}

void printAST(struct TokenList tokens, AbstractSyntaxTree* ast) {
    printf("=== Abstract Syntax Tree ===\n");
    printExpression(ast->expressions, ast->rootExpression, 0);
}

