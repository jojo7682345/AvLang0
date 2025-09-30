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

#define match(input, type, ...) _scan(input, true, type, __VA_ARGS__, TOKEN_TYPE_NONE);
#define check(input, type, ...) _scan(input, false, type, __VA_ARGS__, TOKEN_TYPE_NONE);
#define consume(input, type, ...) _scan(input, true, type, __VA_ARGS__, TOKEN_TYPE_NONE);
static const struct Token* _scan(struct TokenList* input, bool32 advance, enum TokenType type, ...){
	if(input->currentToken >= input->tokenCount){
		return nullptr;
	}
	const Token* token = &input->tokens[input->currentToken];
	if(token->type == type){
		input->currentToken += advance ? 1 : 0;
		return token;
	}
	va_list args;
	va_start(args, type);

	while((type = va_arg(args, enum TokenType))){
		if(token->type == type){
			va_end(args);
			input->currentToken += advance ? 1 : 0;
			return token;
		}
	}
	
	va_end(args);
	return nullptr;
}
#define GET_EXPRESSION(expression) ((struct Expression*)avDynamicArrayGetPtr(expression, *((AvDynamicArray*)(input+1))))

#define BINARY_EXPRESSION(leftExpression, operator, rightExpression) {\
	.type=EXPRESSION_TYPE_BINARY,\
	.tokenStart = GET_EXPRESSION(leftExpression)->tokenStart,\
	.tokenEnd = GET_EXPRESSION(rightExpression)->tokenEnd,\
	.binary = {\
		.left = leftExpression,\
		.type = operator,\
		.right = rightExpression,\
	},\
}
#define CREATE_BINARY_EXPRESSION(left, operator, right) createExpression(input, (struct Expression) BINARY_EXPRESSION(left, operator, right))
static Expression createExpression(struct TokenList* input, struct Expression expression){
	AvDynamicArray mem = *((AvDynamicArray*)(input+1));
	return avDynamicArrayAdd(&expression, mem);
}

static Expression parseExpression(struct TokenList* input);

static Expression parsePrimary(struct TokenList* input){

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
