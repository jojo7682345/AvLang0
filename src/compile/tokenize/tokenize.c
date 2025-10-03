#include "../tokenize.h"
#include <utils.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define AV_LOG_CATEGORY TOKENIZE_CATEGORY
#include <logging.h>

struct TokenScan {
	const char* text;
	enum TokenType type;
	uint64 length; 
};

#define TOKEN_KEYWORD(str, name, index) 	{ .text=str, .type=TOKEN_TYPE_##name, .length=sizeof(str)/sizeof(char)},
#define TOKEN_PUNCTUATOR(str, name, index) 	{ .text=str, .type=TOKEN_TYPE_##name, .length=sizeof(str)/sizeof(char)},
#define TOKEN_LITERAL(str, name, index)		{ .text=str, .type=TOKEN_TYPE_##name, .length=sizeof(str)/sizeof(char)},
#define TOKEN_IDENTIFIER(str, name, index) 	{ .text=str, .type=TOKEN_TYPE_##name, .length=sizeof(str)/sizeof(char)},

static const struct TokenScan tokenList[] = {
	TOKEN_LIST
};

static bool8 tokenMask[sizeof(tokenList)/sizeof(struct TokenScan)] = {0};

#undef TOKEN_KEYWORD
#undef TOKEN_PUNCTUATOR
#undef TOKEN_LITERAL
#undef TOKEN_IDENTIFIER

static uint64 countLines(const char* str){
	uint64 lineCount = 1;
	uint64 length = cstrlen(str);
	for(uint64 i = 0; i < length; i++){
		if(str[i] == '\n'){
			lineCount++;
		}
	}
	return lineCount;
}

static uint64 getWordLength(const char* str, uint64 lineLength){
	for(uint32 i = 0; i < lineLength; i++){
		char c = str[i];
		bool32 isLetter = false;
		bool32 isNumber = false;
		bool32 isUnderscore = false;
		if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')){
			isLetter = true;
		}else if(c >= '0' && c <= '9'){
			isNumber = true;
		}else if(c == '_'){
			isUnderscore = true;
		}

		if(isNumber && i == 0){
			return 0;
		}

		if(!(isNumber || isLetter || isUnderscore)){
			return i;
		}
	}
	return lineLength;
}


void splitLines(const char** lines, uint64* lineLengths, struct StringBuffer input){
	const char** currentLine = lines;
	uint64* currentLineLength = lineLengths;
	*currentLine = input.data;
	for(uint64 i = 0; i < input.length; i++){
		char c = input.data[i];
		if(c == '\n'){
			currentLine++;
			currentLineLength++;
			*currentLine = input.data + i + 1;
			(*currentLineLength) = 0;
			continue;
		}
		(*currentLineLength)++;
	}
}

#define IS_AT_END() (current >= input.length)

#define ADVANCE() (input.data[current++])
#define PEEK() (IS_AT_END()?'\0':input.data[current])
#define PEEK_NEXT() (IS_AT_END()?'\0':(current+1 >= input.length?'\0':(input.data[current+1])))
#define MATCH(expected) (IS_AT_END()?false:(input.data[current]==(expected)?((current++)||true):(false)))


static bool32 isDigit(char c){
	return c>='0' && c<='9';
}

static bool32 isAlpha(char c){
	return (c >= 'a' && c <= 'z') ||
		   (c >= 'A' && c <= 'Z') ||
	 		c == '_';
}

static bool32 isAlphaNumeric(char c){
	return isAlpha(c) || isDigit(c);
}

struct TokenLL {
	Token token;
	struct TokenLL* next;
};

static struct TokenLL* createToken(struct TokenLL** start, struct TokenLL* prev, uint64 character, uint64 line, uint64 offset, struct StringBuffer input, uint64 length, enum TokenType type){
	struct TokenLL* node = malloc(sizeof(struct TokenLL));
	if(node == NULL){
		avAssert(AV_MEMORY_ERROR, AV_SUCCESS, "Out of memory. Buy more ram");
		return nullptr;
	}

	struct TokenLL token = {
		.token = {
			.position = {
				.character = character,
				.line = line,
				.offset = offset,
			},
			.text = {
				.start = input.data + offset,
				.length = length,
				.end = input.data + offset + length-1,
			},
			.type = type,
		},
		.next = nullptr,
	};
	memcpy(node, &token, sizeof(struct TokenLL));
	if(prev){
		(prev)->next = node;
	}
	if(*start==nullptr){
		*start = node;
	}	
	return node;
}

#define addToken(type, start, end) currentToken = createToken(&tokens, currentToken, current-lineStart, line, start, input, end - start, type)

bool32 tokenize(struct CompilerCommand command, struct StringBuffer input, struct TokenList* tokenOut){

	bool32 success = true;

	struct TokenLL* tokens = nullptr;
	struct TokenLL* currentToken = nullptr;

	
	uint64 start = 0;
	uint64 current = 0;
	uint64 line = 0;
	uint64 lineStart = 0;
	while(!IS_AT_END()){
		start = current;
		char c = ADVANCE();

		switch(c){
			case '(': addToken(TOKEN_TYPE_PARENTHESESE_OPEN, start, current); break;
			case ')': addToken(TOKEN_TYPE_PARENTHESESE_CLOSE, start, current); break;
			case '{': addToken(TOKEN_TYPE_BRACE_OPEN, start, current); break;
			case '}': addToken(TOKEN_TYPE_BRACE_CLOSE, start, current); break;
			case '[': addToken(TOKEN_TYPE_BRACKET_OPEN, start, current); break;
			case ']': addToken(TOKEN_TYPE_BRACKET_CLOSE, start, current); break;
			case '#': addToken(TOKEN_TYPE_HASH, start, current); break;
			case ',': addToken(TOKEN_TYPE_COMMA, start, current); break;
			case '.': 
				addToken(MATCH('.')?TOKEN_TYPE_RANGE : TOKEN_TYPE_DOT, start, current); break;
			case '~': addToken(TOKEN_TYPE_LOGICAL_INVERT, start, current); break;
			case '-': 
				if(MATCH('-')){
					addToken(TOKEN_TYPE_DECREMENT, start, current);
					break;
				}
				if(MATCH('=')){
					addToken(TOKEN_TYPE_DECREMENT_ASSIGN, start, current);
					break;
				}
				addToken(TOKEN_TYPE_MINUS, start, current);
				break;
			case '+': 
				if(MATCH('+')){
					addToken(TOKEN_TYPE_INCREMENT, start, current);
					break;
				}
				if(MATCH('=')){
					addToken(TOKEN_TYPE_INCREMENT_ASSIGN, start, current);
					break;
				}
				addToken(TOKEN_TYPE_PLUS, start, current);
				break;
			case ';': addToken(TOKEN_TYPE_SEMICOLON, start, current); break;
			case '*': addToken(TOKEN_TYPE_MULTIPLY, start, current); break;
			case '@': addToken(TOKEN_TYPE_AMPERSANT, start, current); break;
			case '%': addToken(TOKEN_TYPE_MODULUS, start, current); break;
			case '!':
				addToken(MATCH('=')?TOKEN_TYPE_NOT_EQUALS:TOKEN_TYPE_LOGICAL_NOT, start, current);
				break;
			case '=':
				addToken(MATCH('=')?TOKEN_TYPE_EQUALS:TOKEN_TYPE_ASSIGNMENT, start, current);
				break;
			case '<':
				addToken(MATCH('=')?TOKEN_TYPE_LESS_THAN_OR_EQUALS: TOKEN_TYPE_LESS_THAN, start, current);
				break;
			case '>':
				addToken(MATCH('=')?TOKEN_TYPE_GREATER_THAN_OR_EQUALS: TOKEN_TYPE_GREATER_THAN, start, current);
				break;
			case '/':
				if(MATCH('/')){
					while(PEEK()!= '\n' && !IS_AT_END()) ADVANCE();
				}else{
					addToken(TOKEN_TYPE_DIVIDE, start, current);
				}
				break;
			case ' ':
			case '\t':
			case '\r':
			case '\0':
				break;
			case '\"':
				//string
				while(PEEK() != '\"' && PEEK()!='\n' && !IS_AT_END()){
					ADVANCE();
				}

				if(IS_AT_END() ||  PEEK()=='\n'){
					avLog(AV_PARSE_ERROR, "unterminated string");
					success = false;
					break;
				}

				ADVANCE();
				addToken(TOKEN_TYPE_STRING, start+1, current-3);
				break;
			case '\n':
				line++;
				lineStart = current;
				break;
			default:
				if(isDigit(c)){
					while(isDigit(PEEK())) ADVANCE();

					if(PEEK()=='.' && isDigit(PEEK_NEXT())){
						ADVANCE();
						while(isDigit(PEEK())) ADVANCE();
					}

					addToken(TOKEN_TYPE_NUMERIC, start, current);

				}else if(isAlpha(c)){

					while(isAlphaNumeric(PEEK())) ADVANCE();

					enum TokenType type = TOKEN_TYPE_IDENTIFIER;
					for(uint32 i = 0; i < sizeof(tokenList)/sizeof(tokenList[0]);i++){
						if((tokenList[i].type & TOKEN_TYPE_KEYWORD) == 0){
							continue;
						}
						uint64 length = start-current;
						if(length != tokenList[i].length){
							continue;
						}
						if(memcmp(input.data+start, tokenList[i].text, tokenList[i].length)==0){
							type = tokenList[i].type;
							break;
						}
					}

					addToken(type, start, current);

				}else{
					avLog(AV_PARSE_ERROR,"unexpected charachter :\'", (char[]){c,'\0'}, "\'");
					success = false;
				}

				break;
		}
	}
	uint64 tokenCount = 0;
	currentToken = tokens;
	while(currentToken!=nullptr){
		tokenCount++;
		currentToken = currentToken->next;
	}

	Token* tokenBuffer = malloc(tokenCount* sizeof(Token));
	if(tokenBuffer==nullptr){
		avAssert(AV_MEMORY_ERROR, AV_SUCCESS, "Out of memory. Buy more ram");
		return false;
	}

	currentToken = tokens;
	for(uint64 i = 0; i < tokenCount; i++){
		memcpy(tokenBuffer+i, &currentToken->token, sizeof(Token));
		struct TokenLL* next = currentToken->next;
		free(currentToken);
		currentToken = next;
	}

	struct TokenList output = {
		.tokenCount = tokenCount,
		.tokens = tokenBuffer,
		.currentToken = 0,
	};
	memcpy(tokenOut, &output, sizeof(struct TokenList));

	printTokens(output);

	return success;
}

#define TOKEN_KEYWORD(str, name, index) 	"TOKEN_TYPE_"#name,
#define TOKEN_PUNCTUATOR(str, name, index) 	"TOKEN_TYPE_"#name,
#define TOKEN_LITERAL(str, name, index)		"TOKEN_TYPE_"#name,
#define TOKEN_IDENTIFIER(str, name, index) 	"TOKEN_TYPE_"#name,

static const char* tokenNames[] = {
	TOKEN_LIST
	"TOKEN_TYPE_IDENTIFIER",
	"UNKNOWN"
};

#undef TOKEN_KEYWORD
#undef TOKEN_PUNCTUATOR
#undef TOKEN_LITERAL
#undef TOKEN_IDENTIFIER

void printTokens(struct TokenList tokens){

	for(uint64 i = 0; i < tokens.tokenCount; i++){
		Token token = tokens.tokens[i];
		char lineStr[128] = {0};
		char lengthStr[128] = {0};
		const char* typeStr = tokenNames[(sizeof(tokenNames)/sizeof(const char*)-1)];
		char tokStr[128] = {0};
		sprintf(lineStr, "%lu", token.position.line);
		sprintf(lengthStr, "%lu", token.text.length);
		for(uint32 j = 0; j < sizeof(tokenList)/sizeof(struct TokenScan); j++){
			if(tokenList[j].type == token.type){
				typeStr = tokenNames[j];
				break;
			}
			if(token.type & TOKEN_TYPE_IDENTIFIER){
				typeStr = tokenNames[(sizeof(tokenNames)/sizeof(const char*)-2)];
				break;
			}
		}
		memcpy(tokStr, token.text.start, min(127, token.text.length));
		avLog(AV_DEBUG, "Token:\n\tLine: ", lineStr, "\n\tLength: ", lengthStr, "\n\tType: ", typeStr, "\n\tValue: ", tokStr);

	}


}
