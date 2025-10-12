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
	AvAllocator* allocator;
	struct SymbolScope* topLevelScope;
	struct SymbolScope* currentScope;
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
	avLogF(AV_INVALID_SYNTAX, "Token: %.*s", parser->tokens->tokens[parser->tokens->currentToken].text.length, parser->tokens->tokens[parser->tokens->currentToken].text.start);
	avLog(AV_INVALID_SYNTAX, "Syntax Erorr");	
}
#define logSemanticError(parser, ...) _logSemanticError(parser, __VA_ARGS__ __VA_OPT__(,) NULL)
void _logSemanticError(Parser* parser, ...){
	avLog(AV_ALREADY_EXISTS, "Semantic Error");
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
	return parseAssignment(parser);
}


static bool32 tokensEqual(const Token* tokenA, const Token* tokenB){
	if(!(tokenA && tokenB)){
		return false;
	}
	if(tokenA->text.length != tokenB->text.length){
		return false;
	}
	for(uint64 i = 0; i < tokenA->text.length; i++){
		if(tokenA->text.start[i]!=tokenB->text.start[i]){
			return false;
		}
	}
	return true;
}

#define UNDEFINED_TYPE INVALID_TYPE


static enum SymbolType getSymbol(Parser* parser, const Token* identifier){
	struct SymbolScope* scope = parser->currentScope;
	while(scope != NULL){
		for(uint32 index = 0; index < avDynamicArrayGetSize(scope->symbols); index++) { 
			struct Symbol element; 
			avDynamicArrayRead(&element, index, (scope->symbols)); 
			{ 
				if(tokensEqual(identifier, element.identifier)){ 
					return element.type; 
				} 
			} 
		};
		scope = scope->parent;
	}
	return SYMBOL_TYPE_NONE;
}

static bool8 addSymbolToScope(Parser* parser, enum SymbolType type, const Token* identifier){
	if(getSymbol(parser, identifier)!=SYMBOL_TYPE_NONE){
		return false;
	}

	struct SymbolScope* scope = parser->currentScope;
	struct Symbol symbol = {
		.identifier = identifier,
		.type = type,
	};
	avDynamicArrayAdd(&symbol, scope->symbols);

	return true;
}

static Type createNamedType(Parser* parser, struct Type type, const Token* identifier){
	if(identifier != NULL && addSymbolToScope(parser, SYMBOL_TYPE_TYPE, identifier)){
		logSemanticError(parser, identifier);
		return INVALID_TYPE;
	}
	struct NamedType index = {
		.identifier = identifier,
		.type = type,
	};
	return avDynamicArrayAdd(&index, parser->types);
}

static Type createType(Parser* parser, struct Type type){
	return createNamedType(parser, type, NULL);
}

static bool32 nameType(Parser* parser, Type type, const Token* identifier){
	if(!addSymbolToScope(parser, SYMBOL_TYPE_TYPE, identifier)){
		logSemanticError(parser, identifier);
		return false;
	}
	avDynamicArrayAccess(struct NamedType, type, parser->types)->identifier = identifier;
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
	if(match(parser, TOKEN_TYPE_BRACE_OPEN)){
		AvDynamicArray tagsArr = AV_EMPTY;
		avDynamicArrayCreate(0, sizeof(Tag),&tagsArr);
		parseTags(parser, tagsArr);
		if(!consume(parser, TOKEN_TYPE_BRACE_CLOSE)){
			logSyntaxError(parser, TOKEN_TYPE_BRACE_CLOSE);
			return INVALID_STATEMENT;
		}
		tagCount = avDynamicArrayGetSize(tagsArr);
		if(tagCount){
			tags = avAllocatorAllocate(sizeof(Tag)*tagCount, parser->allocator);
			avDynamicArrayReadRange(tags, tagCount, 0, sizeof(Tag), 0, tagsArr);
		}
		avDynamicArrayDestroy(tagsArr);
	}
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
		if(expression==INVALID_EXPRESSION){
			continue;
		}
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
static Statement parseStatement(Parser* parser);
static Statement parseBody(Parser* parser){
	if(!match(parser, TOKEN_TYPE_BRACE_OPEN)){
		return parseStatement(parser);
	}
	uint32 statementCount = 0;
	Statement* statements = NULL;
	AvDynamicArray stmts;
	avDynamicArrayCreate(0, sizeof(Statement), &stmts);
	
	while(!match(parser, TOKEN_TYPE_BRACE_CLOSE)){
		Statement statement = parseStatement(parser);
		if(statement != INVALID_STATEMENT){
			avDynamicArrayAdd(&statement, stmts);
		}
	}

	statementCount = avDynamicArrayGetSize(stmts);
	if(statementCount){
		statements = avAllocatorAllocate(sizeof(Statement)*statementCount, parser->allocator);
		avDynamicArrayReadRange(statements, statementCount, 0, sizeof(Statement), 0, stmts);
	}
	avDynamicArrayDestroy(stmts);

	struct Statement statement = {
		.type = STATEMENT_TYPE_BLOCK,
		.block = {
			.statements = statements,
			.statementCount = statementCount,
		},
	};
	return createStatement(parser, statement);
}

struct SymbolScope* enterScope(Parser* parser){
	struct SymbolScope* scope = avAllocatorAllocate(sizeof(struct SymbolScope), parser->allocator);
	scope->parent = parser->currentScope;
	avDynamicArrayCreate(0, sizeof(struct Symbol), &scope->symbols);
	parser->currentScope = scope;
	return scope;
}

void exitScope(Parser* parser){
	if(parser->currentScope==NULL){
		avAssert(parser->currentScope!=NULL, AV_ERROR, "scope is null");
		return;
	}
	struct SymbolScope* scope = parser->currentScope;
	parser->currentScope = scope->parent;
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
		logSyntaxError(parser, TOKEN_TYPE_PARENTHESESE_OPEN);
		return INVALID_STATEMENT;
	}
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
	
	if(match(parser, TOKEN_TYPE_SEMICOLON)){
		body = INVALID_STATEMENT;
	}else{
		enterScope(parser);
		body = parseBody(parser);
		exitScope(parser);
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
	if(!consume(parser, TOKEN_TYPE_SEMICOLON)){
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
	nameType(parser, type, identifier);
	return createStatement(parser, stmt);
}

static Statement parseExpressionStatement(Parser* parser){
	Expression expression = parseExpression(parser);
	if(expression==INVALID_EXPRESSION){
		consumeUpTo(parser, TOKEN_TYPE_SEMICOLON);
	}
	if(!consume(parser, TOKEN_TYPE_SEMICOLON)){
		logSyntaxError(parser, TOKEN_TYPE_SEMICOLON);
		return INVALID_STATEMENT;
	}
	struct Statement stmt = {
		.type = STATEMENT_TYPE_EXPRESSION,
		.expression = expression,
	};
	return createStatement(parser, stmt);
}

static Statement parseReturnStatement(Parser* parser){
	Expression value = INVALID_EXPRESSION;
	if(!check(parser, TOKEN_TYPE_SEMICOLON)){
		value = parseExpression(parser);
	}
	if(!consume(parser, TOKEN_TYPE_SEMICOLON)){
		logSyntaxError(parser, TOKEN_TYPE_SEMICOLON);
		return INVALID_STATEMENT;
	}
	struct Statement stmt = {
		.type = STATEMENT_TYPE_RETURN,
		.expression = value,
	};
	return createStatement(parser, stmt);
}

static Statement parseStatement(Parser* parser){
	const Token* token;
	if(match(parser, TOKEN_TYPE_TYPEDEF)){
		return parseTypedef(parser);
	}
	if((token = check(parser, TOKEN_TYPE_IDENTIFIER))){
		enum SymbolType type = getSymbol(parser, token);
		if(type == SYMBOL_TYPE_TYPE){
			return parseDeclaration(parser);
		}else{
			return parseExpressionStatement(parser);
		}
	}
	if(match(parser, TOKEN_TYPE_RETURN)){
		return parseReturnStatement(parser);
	}
	return parseExpressionStatement(parser);
}

static void parseStatements(Parser* parser, AvDynamicArray statementList){
	while(parser->tokens->currentToken < parser->tokens->tokenCount){
		Statement statement = parseStatement(parser);
		if(statement == INVALID_STATEMENT){
			continue;
		}
		avDynamicArrayAdd(&statement, statementList);
	}
}

bool32 parse(struct CompilerCommand command, struct TokenList input, AbstractSyntaxTree* ast){	
	bool32 success = true;

	AvAllocator allocator = AV_EMPTY;
	Parser parser = {
		.tokens = &input,
		.allocator = &ast->allocator,
	};
	avDynamicArrayCreate(0, sizeof(struct Expression), &parser.expressions);
	avDynamicArrayCreate(0, sizeof(struct Statement), &parser.statements);
	avDynamicArrayCreate(0, sizeof(struct NamedType), &parser.types);
	avAllocatorCreate(0, AV_ALLOCATOR_TYPE_DYNAMIC, parser.allocator);


	AvDynamicArray rootStatements;
	avDynamicArrayCreate(0, sizeof(Statement), &rootStatements);

	enterScope(&parser);
	parseStatements(&parser, rootStatements);
	exitScope(&parser);
	avAssert(parser.currentScope==NULL, AV_ERROR, "Scope inbalance");
	
	uint32 rootStatementCount = avDynamicArrayGetSize(rootStatements);
	if(!rootStatementCount){
		success = false;
		goto exitParse;
	}
	
	ast->rootStatements = avCallocate(rootStatementCount, sizeof(Statement), "");
	avDynamicArrayReadRange(ast->rootStatements, rootStatementCount, 0, sizeof(Statement), 0, rootStatements);
	ast->rootStatementCount = rootStatementCount;

	uint32 statementCount = avDynamicArrayGetSize(parser.statements);
	ast->statements = avCallocate(statementCount, sizeof(struct Statement), "");
	avDynamicArrayReadRange(ast->statements, statementCount, 0, sizeof(struct Statement), 0, parser.statements);

	uint32 typeCount = avDynamicArrayGetSize(parser.types);
	if(typeCount){
		ast->types = avCallocate(typeCount, sizeof(struct NamedType), "");
		avDynamicArrayReadRange(ast->types, typeCount, 0, sizeof(struct NamedType), 0, parser.types);
	}
	uint32 expressionCount = avDynamicArrayGetSize(parser.expressions);
	if(expressionCount){
		ast->expressions = avCallocate(expressionCount, sizeof(struct Expression), "");
		avDynamicArrayReadRange(ast->expressions, expressionCount, 0, sizeof(struct Expression), 0, parser.expressions);
	}

	printAST(ast);
exitParse:
	avDynamicArrayDestroy(rootStatements);
	avDynamicArrayDestroy(parser.expressions);
	avDynamicArrayDestroy(parser.statements);
	avDynamicArrayDestroy(parser.types);
	return true;
}
// Helper for indentation
static void printIndent(int level) {
    for (int i = 0; i < level; ++i) {
        avLogF(AV_DEBUG,"  ");
    }
}

// Recursive expression printer
static void printExpression(struct Expression* expressions, Expression index, int indentLevel) {
    struct Expression* expr = expressions + index;
    if (!expr) {
        printIndent(indentLevel);
        avLogF(AV_DEBUG,"<invalid expression>\n");
        return;
    }

    printIndent(indentLevel);
    switch (expr->type) {
        case EXPRESSION_TYPE_NUMBER_LITERAL:
            avLogF(AV_DEBUG,"NumberLiteral: %.*s\n",
                (int)expr->numeric->text.length,
                expr->numeric->text.start);
            break;

        case EXPRESSION_TYPE_STRING_LITERAL:
            avLogF(AV_DEBUG,"StringLiteral: \"%.*s\"\n",
                (int)expr->string->text.length,
                expr->string->text.start);
            break;

        case EXPRESSION_TYPE_IDENTIFIER:
            avLogF(AV_DEBUG,"Identifier: %.*s\n",
                (int)expr->identifier->text.length,
                expr->identifier->text.start);
            break;

        case EXPRESSION_TYPE_UNARY:
            avLogF(AV_DEBUG,"UnaryExpression (%d):\n", expr->unary.type);
            printExpression(expressions, expr->unary.inner, indentLevel + 1);
            break;

        case EXPRESSION_TYPE_BINARY:
            avLogF(AV_DEBUG,"BinaryExpression (%d):\n", expr->binary.type);
            printIndent(indentLevel + 1);
            avLogF(AV_DEBUG,"Left:\n");
            printExpression(expressions, expr->binary.left, indentLevel + 2);
            printIndent(indentLevel + 1);
            avLogF(AV_DEBUG,"Right:\n");
            printExpression(expressions, expr->binary.right, indentLevel + 2);
            break;

        case EXPRESSION_TYPE_NONE:
            avLogF(AV_DEBUG,"Invalid/Empty Expression\n");
            break;

        default:
            avLogF(AV_DEBUG,"Unknown Expression Type (%d)\n", expr->type);
            break;
    }
}

void printType(AbstractSyntaxTree* ast, Type typeHandle, int indentLevel){
	struct NamedType type = ast->types[typeHandle];
	printIndent(indentLevel);
	avLogF(AV_DEBUG,"Base: %.*s\n", (int)type.type.identifier->text.length, type.type.identifier->text.start);
	if(type.type.tagCount){
		printIndent(indentLevel);
		avLogF(AV_DEBUG,"Tags: [\n");
		for(uint32 i = 0; i < type.type.tagCount; i++){
			printExpression(ast->expressions, type.type.tags[i], indentLevel+1);
		}
		printIndent(indentLevel);
		avLogF(AV_DEBUG,"]\n");
	}
}

void printStatement(AbstractSyntaxTree* ast, Statement handle, int indentLevel){
	struct Statement statement = ast->statements[handle];

	printIndent(indentLevel);
	switch(statement.type){
		case STATEMENT_TYPE_TYPEDEF:
			avLogF(AV_DEBUG,"Typedef: {\n");
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Type: {\n");
			printType(ast, statement.typeDefine.type, indentLevel+2);
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Name: {\n");
			printIndent(indentLevel+2);
			avLogF(AV_DEBUG,"%.*s\n", (int)statement.typeDefine.identifier->text.length, statement.typeDefine.identifier->text.start );
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"}\n");
			break;
		case STATEMENT_TYPE_FUNCTION:
			avLogF(AV_DEBUG,"Function: {\n");
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Name: {\n");
			printIndent(indentLevel+2);
			avLogF(AV_DEBUG,"%.*s\n", (int)statement.function.identifier->text.length, statement.function.identifier->text.start );
			if(statement.function.parameterCount){
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"Parameters: [\n");
				for(uint32 i = 0; i < statement.function.parameterCount; i++){
					printStatement(ast, statement.function.parameters[i], indentLevel+2);
				}
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"]\n");
			}
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"ReturnType: {\n");
			printType(ast, statement.function.returnType, indentLevel+2);
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			if(statement.function.tagCount){
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"Tags: [\n");
				for(uint32 i = 0; i < statement.function.tagCount; i++){
					printExpression(ast->expressions, statement.function.tags[i], indentLevel+2);
				}
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"]\n");
			}
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Body: {\n");
			printStatement(ast, statement.function.body, indentLevel+2);
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"}\n");
			break;
		case STATEMENT_TYPE_RETURN:
			avLogF(AV_DEBUG,"Return: {\n");
			printExpression(ast->expressions, statement.expression, indentLevel+1);
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"}\n");
			break;
		case STATEMENT_TYPE_BLOCK:
			avLogF(AV_DEBUG,"Statements: [\n");
			for(uint32 i = 0; i < statement.block.statementCount; i++){
				printStatement(ast, statement.block.statements[i], indentLevel+1);
			}
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"]\n");
			break;
		case STATEMENT_TYPE_DECLARATION:
			avLogF(AV_DEBUG,"Declaration: {\n");
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Type: {\n");
			printType(ast, statement.declaration.type, indentLevel+2);
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"Name: {\n");
			printIndent(indentLevel+2);
			avLogF(AV_DEBUG,"%.*s\n", (int)statement.declaration.identifier->text.length, statement.declaration.identifier->text.start);
			printIndent(indentLevel+1);
			avLogF(AV_DEBUG,"}\n");
			if(statement.declaration.initializer!=INVALID_EXPRESSION){
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"Value: {\n");
				printExpression(ast->expressions, statement.declaration.initializer, indentLevel+2);
				printIndent(indentLevel+1);
				avLogF(AV_DEBUG,"}\n");
			}
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"}\n");
			break;
		case STATEMENT_TYPE_EXPRESSION:
			avLogF(AV_DEBUG,"Expression: {\n");
			printExpression(ast->expressions, statement.expression, indentLevel+1);
			printIndent(indentLevel);
			avLogF(AV_DEBUG,"}\n");
			break;
		default:
			avLogF(AV_DEBUG,"Unknown Statement Type(%d)\n", statement.type);
			break;
	}
}

void printAST(AbstractSyntaxTree* ast) {
    avLogF(AV_DEBUG,"=== Abstract Syntax Tree ===\n");
    for(uint32 i = 0; i < ast->rootStatementCount; i++){
		printStatement(ast, ast->rootStatements[i], 0);
	}
}

