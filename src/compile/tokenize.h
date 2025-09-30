#ifndef __TOKENIZE_H__
#define __TOKENIZE_H__

#include <types.h>
#include <compilerState.h>
#include "load.h"

#define TOKEN_LIST\
	TOKEN_KEYWORD("if",				IF,							0x00)\
	TOKEN_KEYWORD("else",			ELSE,						0x01)\
	TOKEN_KEYWORD("while",			WHILE,						0x02)\
	TOKEN_KEYWORD("for",			FOR,						0x03)\
	TOKEN_KEYWORD("break",			BREAK,						0x04)\
	TOKEN_KEYWORD("return",			RETURN,						0x05)\
	TOKEN_KEYWORD("struct",			STRUCT,						0x06)\
	TOKEN_KEYWORD("enum",			ENUM,						0x07)\
	TOKEN_KEYWORD("typedef",		TYPEDEF,					0x08)\
	TOKEN_KEYWORD("continue",		CONTINUE,					0x08)\
	\
	TOKEN_PUNCTUATOR("+",			PLUS, 						0x00)\
	TOKEN_PUNCTUATOR("-",			MINUS, 						0x01)\
	TOKEN_PUNCTUATOR("*",			MULTIPLY, 					0x02)\
	TOKEN_PUNCTUATOR("/",			DIVIDE,						0x03)\
	TOKEN_PUNCTUATOR("!",			LOGICAL_NOT,				0x04)\
	TOKEN_PUNCTUATOR("&",			AND,						0x05)\
	TOKEN_PUNCTUATOR("|",			OR,							0x06)\
	TOKEN_PUNCTUATOR("&&",			LOGICAL_AND,				0x07)\
	TOKEN_PUNCTUATOR("||", 			LOGICAL_OR,					0x08)\
	TOKEN_PUNCTUATOR("(",			PARENTHESESE_OPEN,			0x09)\
	TOKEN_PUNCTUATOR(")",			PARENTHESESE_CLOSE,			0x0A)\
	TOKEN_PUNCTUATOR(";",			SEMICOLON,					0x0B)\
	TOKEN_PUNCTUATOR("=",			ASSIGNMENT,					0x0C)\
	TOKEN_PUNCTUATOR("==",			EQUALS,						0x0D)\
	TOKEN_PUNCTUATOR("!=",			NOT_EQUALS,					0x0E)\
	TOKEN_PUNCTUATOR(">",			GREATER_THAN,				0x0F)\
	TOKEN_PUNCTUATOR(">=",			GREATER_THAN_OR_EQUALS,		0x10)\
	TOKEN_PUNCTUATOR("<",			LESS_THAN,					0x11)\
	TOKEN_PUNCTUATOR("<=",			LESS_THAN_OR_EQUALS,		0x12)\
	TOKEN_PUNCTUATOR("{",			BRACE_OPEN,					0x13)\
	TOKEN_PUNCTUATOR("}",			BRACE_CLOSE,				0x14)\
	TOKEN_PUNCTUATOR("[",			BRACKET_OPEN,				0x15)\
	TOKEN_PUNCTUATOR("]",			BRACKET_CLOSE,				0x16)\
	TOKEN_PUNCTUATOR(",",			COMMA,						0x17)\
	TOKEN_PUNCTUATOR(".",			DOT,						0x18)\
	TOKEN_PUNCTUATOR("#",			HASH,						0x19)\
	TOKEN_PUNCTUATOR("@",			AMPERSANT,					0x20)\
	\
	TOKEN_LITERAL("",				NUMERIC,					0x00)\
	TOKEN_LITERAL("",				CHARACTER,					0x01)\
	TOKEN_LITERAL("",				STRING,						0x02)
	
#define TOKEN_KEYWORD(str, name, index) 	TOKEN_TYPE_##name = (index+1) | TOKEN_TYPE_KEYWORD,
#define TOKEN_PUNCTUATOR(str, name, index) 	TOKEN_TYPE_##name = (index+1) | TOKEN_TYPE_PUNCTUATOR,
#define TOKEN_LITERAL(str, name, index) 	TOKEN_TYPE_##name = (index+1) | TOKEN_TYPE_LITERAL,
#define TOKEN_IDENTIFIER(str, name, index) 	TOKEN_TYPE_##name = (index+1) | TOKEN_TYPE_IDENTIFIER,

enum TokenType {
	TOKEN_TYPE_NONE = 0,
	TOKEN_TYPE_KEYWORD 		= 0x80000000,
	TOKEN_TYPE_PUNCTUATOR	= 0x40000000,
	TOKEN_TYPE_LITERAL		= 0x20000000,
	TOKEN_TYPE_IDENTIFIER	= 0x10000000,
	TOKEN_LIST
};

#undef TOKEN_KEYWORD
#undef TOKEN_PUNCTUATOR
#undef TOKEN_LITERAL
#undef TOKEN_IDENTIFIER

struct TextPosition {
	uint64 offset;
	uint64 line;
	uint64 character;
};

struct StringSlice {
	const char* const start;
	const char* const end;
	const uint64 length; 
};

typedef struct Token {
	const enum TokenType type;
	const struct TextPosition position;
	const struct StringSlice text;
} Token;

struct TokenList {
	const struct Token* tokens;
	const uint64 tokenCount;
	const uint64 currentToken;
};

bool32 tokenize(struct CompilerCommand command, struct StringBuffer input, struct TokenList* tokens);

void printTokens(struct TokenList tokens);

#endif//__TOKENIZE_H__
