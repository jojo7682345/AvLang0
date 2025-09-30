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

void _logSyntaxError(Parser* parser,)

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
	}
	
}

bool32 parse(struct CompilerCommand command, struct TokenList input, AbstractSyntaxTree* ast){	
	struct {
		struct TokenList input;
		AvDynamicArray astMemory;
	} scan = {
		.input = input,
	};
	avDynamicArrayCreate(0, sizeof(struct Expression), &scan.astMemory);
	
	

	return true;
}

void printAST(struct TokenList tokens, AbstractSyntaxTree* ast){
	
}
