#include "../tokenize.h"
#include "types.h"
#include "../parse.h"
#include <AvUtils/avDefinitions.h>
#include <utils.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <AvUtils/avTypes.h>
#define AV_DYNAMIC_ARRAY_ADVANCED
#define AV_DYNAMIC_ARRAY_EXPOSE_MEMORY_LAYOUT
#include <AvUtils/dataStructures/avDynamicArray.h>
#include <AvUtils/memory/avAllocator.h>
#include <AvUtils/avMemory.h>
#define AV_LOG_CATEGORY PARSE_CATEGORY
#include <logging.h>


typedef struct AbstractSyntaxTree AbstractSyntaxTree;
typedef struct Parser {
	struct TokenList* tokens;
	AvDynamicArray expressions;
	AvDynamicArray statements;
	AvDynamicArray types;
	AvDynamicArray tags;
	AvAllocator* allocator;
}Parser;



#define match(input, type, ...) _scan(input, true, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define check(input, type, ...) _scan(input, false, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define consume(input, type, ...) _scan(input, true, type, __VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)
#define consumeUpTo(input, type, ...) while(!check(input,type,__VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)){input->tokens->currentToken += 1;}
#define consumeUpToAndIncluding(input, type, ...) while(!match(input,type,__VA_ARGS__ __VA_OPT__(,) TOKEN_TYPE_NONE)){input->tokens->currentToken += 1; }
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

static Expression createExpression(Parser* parser, struct Expression expression){
	return avDynamicArrayAdd(&expression, parser->expressions);
}

static Statement createStatement(Parser* parser, struct Statement statement){
	return avDynamicArrayAdd(&statement, parser->statements);
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

static Expression parseRange(Parser* parser){
	Expression left = parseComparison(parser);

	const Token* token;
	if((token = match(parser, 
		TOKEN_TYPE_RANGE,
	))){
		enum BinaryType operator;
		switch(token->type){
			case TOKEN_TYPE_RANGE:
				operator = BINARY_TYPE_RANGE;
				break;			
			default: return INVALID_EXPRESSION;
		}

		Expression right = parseComparison(parser);
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

static Expression parseAssignment(Parser* parser){	
	Expression left = parseRange(parser);

	const Token* token;
	while((token = match(parser, 
		TOKEN_TYPE_ASSIGNMENT, TOKEN_TYPE_INCREMENT_ASSIGN, TOKEN_TYPE_DECREMENT_ASSIGN
	))){
		enum BinaryType operator;
		switch(token->type){
			case TOKEN_TYPE_ASSIGNMENT:
				operator = BINARY_TYPE_ASSIGN;
				break;
			case TOKEN_TYPE_INCREMENT_ASSIGN:
				operator = BINARY_TYPE_INC_ASSIGN;
				break;
			case TOKEN_TYPE_DECREMENT_ASSIGN:
				operator = BINARY_TYPE_DEC_ASSIGN;
				break;
			default: return INVALID_EXPRESSION;
		}

		Expression right = parseRange(parser);
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

static Type createNamedType(Parser* parser, struct Type type, const Token* identifier){

}

static Type createType(Parser* parser, struct Type type){
	return createNamedType(parser, type, NULL);
}

static void parseTags(Parser* parser, AvDynamicArray tags);
static Type parseType(Parser* parser){
	const Token* baseType = consume(parser, TOKEN_TYPE_IDENTIFIER);
	if(baseType==NULL){
		logSyntaxError(parser, TOKEN_TYPE_IDENTIFIER);
		return INVALID_TYPE;
	}
	uint32 tagCount = 0;
	Tag* tags = NULL;
	AvDynamicArray tagsArr = AV_EMPTY;
	avDynamicArrayCreate(0, sizeof(Tag),&tagsArr);
	parseTags(parser, tagsArr);
	if(tagCount){
		tags = avAllocatorAllocate(sizeof(Tag)*tagCount, parser->allocator);
		avDynamicArrayReadRange(tags, tagCount, 0, sizeof(Tag), 0, tagsArr);
	}
	avDynamicArrayDestroy(tagsArr);
	struct Type type = {
		.identifier = baseType,
		.tagCount = tagCount,
		.tags = tags,
	};
	return createType(parser, type);
}

static void parseTags(Parser* parser, AvDynamicArray tags){
	do{
		if(check(parser, TOKEN_TYPE_BRACE_CLOSE)){
			break;
		}
		Expression expression = parseAssignment(parser);
		avDynamicArrayAdd(&expression, tags);	
	}while(match(parser, TOKEN_TYPE_SEMICOLON));
}
static Statement parseVariable(Parser* parser, Type type, const Token* identifier);
static void parseFunctionParameters(Parser* parser, AvDynamicArray params){
	do{
		Type type = parseType(parser);
		const Token* identifier = consume(parser, TOKEN_TYPE_IDENTIFIER);
		if(identifier == NULL){
			logSyntaxError(parser, TOKEN_TYPE_IDENTIFIER);
			consumeUpTo(parser, TOKEN_TYPE_COMMA);
			continue;
		}
		Statement param = parseVariable(parser, type, identifier);
		avDynamicArrayAdd(&param, params);
	}while(match(parser, TOKEN_TYPE_COMMA));
}

static Statement parseFunction(Parser* parser, Type type, const Token* identifier){
	uint32 functionTagCount = 0;
	Tag* functionTags = nullptr;
	uint32 parameterCount = 0;
	Statement* parameters = nullptr;
	Statement body = INVALID_STATEMENT;
	if(match(parser, TOKEN_TYPE_BRACE_OPEN)){
		AvDynamicArray tags = AV_EMPTY;
		avDynamicArrayCreate(0, sizeof(Tag), &tags);
		parseTags(parser, tags);
		functionTagCount = avDynamicArrayGetSize(tags);
		if(functionTagCount){
			functionTags = avAllocatorAllocate(sizeof(Tag)*functionTagCount, parser->allocator);
			avDynamicArrayReadRange(functionTags, functionTagCount, 0, sizeof(Tag), 0, tags);
		}
		avDynamicArrayDestroy(tags);
		if(!consume(parser, TOKEN_TYPE_BRACE_CLOSE)){
			logSyntaxError(parser, TOKEN_TYPE_BRACE_CLOSE);
			return INVALID_STATEMENT;
		}
	}
	if(!consume(parser, TOKEN_TYPE_PARENTHESESE_OPEN)){
		AvDynamicArray params = AV_EMPTY;
		avDynamicArrayCreate(0, sizeof(FunctionParameter), &params);
		parseFunctionParameters(parser, params);
		parameterCount = avDynamicArrayGetSize(params);
		if(parameterCount){
			parameters = avAllocatorAllocate(sizeof(FunctionParameter)*parameterCount, parser->allocator);
			avDynamicArrayReadRange(parameters, parameterCount, 0, sizeof(FunctionParameter), 0, params);
		}
		avDynamicArrayDestroy(params);
		if(!consume(parser,TOKEN_TYPE_PARENTHESESE_CLOSE)){
			logSyntaxError(parser, TOKEN_TYPE_PARENTHESESE_CLOSE);
			return INVALID_STATEMENT;
		}
	}
	if(match(parser, TOKEN_TYPE_SEMICOLON)){
		body = INVALID_STATEMENT;
	}else{
		body = parseBody(parser);
	}

	struct Statement stmt = {
		.type = STATEMENT_TYPE_FUNCTION,
		.function = {
			.returnType = type,
			.identifier = identifier,
			.tagCount = functionTagCount,
			.tags = functionTags,
			.parameterCount = parameterCount,
			.parameters = parameters,
			.body = body,
		},
	};
	return createStatement(parser, stmt);
}

static Statement parseVariable(Parser* parser, Type type, const Token* identifier){
	Expression value = INVALID_EXPRESSION;
	if(match(parser, TOKEN_TYPE_EQUALS)){
		value = parseExpression(parser);
	}
	struct Statement stmt = {
		.type = STATEMENT_TYPE_DECLARATION,
		.declaration = {
			.type = type,
			.identifier = identifier,
			.initializer = value,
		},
	};
	return createStatement(parser, stmt);
}

static Statement parseDeclaration(Parser* parser){
	Type type = parseType(parser);
	const Token* identifier = consume(parser, TOKEN_TYPE_IDENTIFIER);
	if(identifier==NULL){
		logSyntaxError(parser, TOKEN_TYPE_IDENTIFIER);
		return INVALID_STATEMENT;
	}
	if(check(parser, TOKEN_TYPE_BRACE_OPEN, TOKEN_TYPE_PARENTHESESE_OPEN)){
		return parseFunction(parser, type, identifier);	
	}else{
		Statement var = parseVariable(parser, type, identifier);
		if(!consume(parser, TOKEN_TYPE_SEMICOLON)){
			logSyntaxError(parser, TOKEN_TYPE_SEMICOLON);
			return INVALID_STATEMENT;
		}
		return var;
	}

}

static Statement parseTypedef(Parser* parser){
	Type type = parseType(parser);
	const Token* identifier = consume(parser, TOKEN_TYPE_IDENTIFIER);
	if(identifier==NULL){
		logSyntaxError(parser,TOKEN_TYPE_IDENTIFIER);
		consumeUpToAndIncluding(parser,TOKEN_TYPE_SEMICOLON);
		return INVALID_STATEMENT;
	}
	if(!consume(parser, TOKEN_TYPE_IDENTIFIER)){
		logSyntaxError(parser, TOKEN_TYPE_SEMICOLON);
		return INVALID_STATEMENT;
	}
	struct Statement stmt = {
		.type = STATEMENT_TYPE_TYPEDEF,
		.typeDefine = {
			.type = type,
			.identifier = identifier,
		},
	};
	return createStatement(parser, stmt);
}

static Statement parseExpressionStatement(Parser* parser){
	Expression expression = parseExpression(parser);
	if(expression==INVALID_EXPRESSION){
		consumeUpTo(parser, TOKEN_TYPE_SEMICOLON);
	}
	if(!consume(parser, TOKEN_TYPE_SEMICOLON)){
		logSyntaxError(parser, TOKEN_TYPE_SEMICOLON);
		return INVALID_EXPRESSION;
	}
	struct Statement stmt = {
		.type = STATEMENT_TYPE_EXPRESSION,
		.expression = expression,
	};
	return createStatement(parser, stmt);
}

static Statement parseStatement(Parser* parser){
	const Token* token;
	if(match(parser, TOKEN_TYPE_TYPEDEF)){
		return parseTypedef(parser);
	}
	if((token = check(parser, TOKEN_TYPE_IDENTIFIER))){
		if(isNamedType(parser, token)){
			return parseDeclaration(parser);
		}else{
			return parseExpressionStatement(parser);
		}
	}


}

bool32 parse(struct CompilerCommand command, struct TokenList input, AbstractSyntaxTree* ast){	

	Parser parser = {
		.tokens = &input,
	};
	avDynamicArrayCreate(0, sizeof(struct Expression), &parser.expressions);
	avDynamicArrayCreate(0, sizeof(struct Statement), &parser.statements);
	avDynamicArrayCreate(0, sizeof(struct Type), &parser.types);
	
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

