#include "cli.h"
#include <utils.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>

#define AV_LOG_CATEGORY "cli"
#include <logging.h>

struct ParseFlags {
		uint32 inputFileComplete : 1;
		uint32 outputFileComplete : 1;
};

struct Arguments {
	const int32 argC;
	const char** argV;
	const char** current;
};

const char* get(struct Arguments* args){
	if((((uint64)args->current - (uint64)args->argV)/sizeof(const char*)) < args->argC){
		return *(args->current++);
	}
	return NULL;
}

const char* peek(struct Arguments args){
	if((((uint64)args.current - (uint64)args.argV)/sizeof(const char*)) < args.argC){
		return *args.current;
	}
	return NULL;
}

struct CompilerCommandStatusFlags parseCompileOnlyFlag(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.outputType != COMPILER_OUTPUT_TYPE_DEFAULT){
		status.errors.multipleOutputTypesSpecified = 1;
		return status;
	}
	command->flags.outputType = COMPILER_OUTPUT_TYPE_OBJECT;
	return status;
}

struct CompilerCommandStatusFlags parseIrOnlyFlag(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.outputType != COMPILER_OUTPUT_TYPE_DEFAULT){
		status.errors.multipleOutputTypesSpecified = 1;
		return status;
	}
	command->flags.outputType = COMPILER_OUTPUT_TYPE_IR;
	return status;
}

struct CompilerCommandStatusFlags parseVerbosityLevelSilent(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.verbosityLevel != VERBOSITY_LEVEL_DEFAULT){
		status.warnings.multipleVerbosityLevelsSpecified = 1;
		return status;
	}
	command->flags.verbosityLevel = VERBOSITY_LEVEL_SILENT;
	return status;
}

struct CompilerCommandStatusFlags parseVerbosityLevelInfo(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.verbosityLevel != VERBOSITY_LEVEL_DEFAULT){
		status.warnings.multipleVerbosityLevelsSpecified = 1;
		return status;
	}
	command->flags.verbosityLevel = VERBOSITY_LEVEL_INFO;
	return status;
}

struct CompilerCommandStatusFlags parseVerbosityLevelSuccess(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.verbosityLevel != VERBOSITY_LEVEL_DEFAULT){
		status.warnings.multipleVerbosityLevelsSpecified = 1;
		return status;
	}
	command->flags.verbosityLevel = VERBOSITY_LEVEL_SUCCESS;
	return status;
}

struct CompilerCommandStatusFlags parseVerbosityLevelDebug(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.verbosityLevel != VERBOSITY_LEVEL_DEFAULT){
		status.warnings.multipleVerbosityLevelsSpecified = 1;
		return status;
	}
	command->flags.verbosityLevel = VERBOSITY_LEVEL_DEBUG;
	return status;
}

struct CompilerCommandStatusFlags parseDebugParse(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.stageDebugFlags.parse){
		status.warnings.multipleDebugStagesSpecified = 1;
		return status;
	}
	command->flags.stageDebugFlags.parse = 1;
	return status;
}


struct CompilerCommandStatusFlags parseDebugTokenize(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.stageDebugFlags.tokenize){
		status.warnings.multipleDebugStagesSpecified = 1;
		return status;
	}
	command->flags.stageDebugFlags.tokenize = 1;
	return status;
}

struct CompilerCommandStatusFlags parseDebugLoad(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	if(command->flags.stageDebugFlags.load){
		status.warnings.multipleDebugStagesSpecified = 1;
		return status;
	}
	command->flags.stageDebugFlags.load = 1;
	return status;
}


struct CompilerCommandStatusFlags parseOutputFile(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){
	struct CompilerCommandStatusFlags status = {0};
	
	if(parseFlags->outputFileComplete == 1){
		status.errors.multipleOutputFilesSpecified = 1;
		return status;
	}

	const char* outputFile = get(args);
	if(outputFile == NULL){
		status.errors.invalidCommand = 1;
		return status;
	}

	uint64 len = cstrlen(outputFile);
	if(len==0){
		status.errors.invalidCommand = 1;
		return status;
	}

	if(len >= sizeof(command->inputFile)){
		status.warnings.inputStringTooLong = 1;
		status.infos.inputStringTruncated = 1;
	}

	uint64 length = min(len, sizeof(command->outputFile) - 1);
	copyMem(outputFile, command->outputFile, length);
	((char*)command->outputFile)[length] = '\0';
	parseFlags->outputFileComplete = 1;
	return status;
}


struct CompilerCommandStatusFlags parseHelpFlag(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){

	struct CompilerCommandStatusFlags status = {0};
	command->flags.printHelp = true;
	return status;
}


const struct CompilerFlag{
	struct CompilerCommandStatusFlags (*parse)(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags);
	const char* shortName;
	const char* longName;
	const char* usage;
	const char* description;
} flags[] = {
	{.parse=parseCompileOnlyFlag, 		.shortName="c",	.longName=NULL, 		.usage=NULL, 		.description = "Compile and assemble, but do not link."},
	{.parse=parseIrOnlyFlag,			.shortName="i",	.longName="output-ir", 	.usage=NULL,		.description = "Compile to IR representation."},
	{.parse=parseOutputFile,			.shortName="o",	.longName=NULL,			.usage="<file>",	.description = "Place the output into <file>."},
	{.parse=parseVerbosityLevelSilent,	.shortName=NULL,.longName="silent",		.usage=NULL,		.description = "Disable compiler printing (not implemented yet)"},
	{.parse=parseVerbosityLevelInfo,	.shortName=NULL,.longName="v-info",		.usage=NULL,		.description = "Prints info messages"},
	{.parse=parseVerbosityLevelSuccess,	.shortName=NULL,.longName="v-success",	.usage=NULL,		.description = "Prints success messages"},
	{.parse=parseVerbosityLevelDebug,	.shortName=NULL,.longName="v-debug",	.usage=NULL,		.description = "Prints debug messages"},
	{.parse=parseDebugLoad,				.shortName=NULL,.longName="d-load",		.usage=NULL,		.description = "disable load stage messages"},
	{.parse=parseDebugTokenize,			.shortName=NULL,.longName="d-tokenize",	.usage=NULL,		.description = "disable tokenize stage messages"},
	{.parse=parseDebugParse,			.shortName=NULL,.longName="d-parse", 	.usage=NULL,		.description = "disable parsing stage messages"},
	{.parse=parseHelpFlag,				.shortName="h",	.longName="help",		.usage=NULL,		.description = "prints this help menu"},
};
const uint32 flagCount = sizeof(flags)/sizeof(struct CompilerFlag);
#ifndef PROJECT_NAME
#define PROJECT_NAME "AvLang"
#endif
void printHelp(){
	printStr("Usage: " PROJECT_NAME " [options] file...\n");
	printStr("Options:\n");
	for(uint32 i = 0; i < flagCount; i++){
		
		if(flags[i].shortName!=NULL){
			printStr("\t");
			printStr("-");
			printStr(flags[i].shortName);
			if(flags[i].usage!=NULL){
				printStr(" ");
				printStr(flags[i].usage);
			}
		}

		if(flags[i].longName!=NULL){
			printStr("\t");
			printStr("--");
			printStr(flags[i].longName);
			if(flags[i].usage!=NULL){
				printStr(" ");
				printStr(flags[i].usage);
			}
		}

		if(flags[i].description!=NULL){
			printStr("\t\t");
			printStr(flags[i].description);
		}

		printStr("\n");
	}	
}


uint64 checkArgument(const char** str, uint64 len, bool32* shortFlag, bool32* longFlag){
	if(len >= 2){
		if((*str)[0] =='-' && (*str)[1]=='-'){
			*longFlag = true;
			*str+=2;
			return len-2;
		}else if ((*str)[0]=='-'){
			*shortFlag = true;
			*str+=1;
			return len-1;
		}
		return len;
	}else if(len >= 1){
		if((*str)[0] =='-'){
			*shortFlag = true;
			*str+=1;
			return len-1;
		}
		return len;
	}
	return 0;
}

struct CompilerCommandStatusFlags parseArgument(struct Arguments* args, struct CompilerCommand* command, struct ParseFlags* parseFlags){

	struct CompilerCommandStatusFlags status = {0};

	const char* current = get(args);
	if(current == NULL){
		return status;
	}
	uint64 len = cstrlen(current);
	if(len==0){
		status.errors.unrecognizedFlag = 1;
		return status;
	}

	bool32 shortFlag = false;
	bool32 longFlag = false;

	uint64 flagLength = checkArgument(&current, len, &shortFlag, &longFlag);
	if(flagLength==0){
		status.errors.unrecognizedFlag = 1;
		return status;
	}

	if(shortFlag && longFlag){
		exit(-1); // logic error in checkArgument
	}

	if(!shortFlag && !longFlag){
		// input file
		if(parseFlags->inputFileComplete){
			status.errors.inputFileAlreadySpecified = 1;
			return status;
		}

		if(len >= sizeof(command->inputFile)){
			status.warnings.inputStringTooLong = 1;
			status.infos.inputStringTruncated = 1;
		}

		uint64 length = min(len, sizeof(command->inputFile) - 1);
		copyMem(current, command->inputFile, length);
		((char*)command->inputFile)[length] = '\0';

		parseFlags->inputFileComplete = 1;

		return status;
	}

	uint64 offset = shortFlag ? offsetof(struct CompilerFlag, shortName) : offsetof(struct CompilerFlag, longName);

	for(uint32 flagIndex = 0; flagIndex < flagCount; flagIndex++){
		const char* flagName = *(const char**)(((byte*)(flags + flagIndex))+offset);
		if(flagName==NULL){
			continue;
		}
		if(!streq(current, flagName)){
			continue;
		}

		return flags[flagIndex].parse(args, command, parseFlags);
	}

	avLog(AV_ERROR, "Unknown Argument");
	status.errors.unrecognizedFlag = 1;
	return status;
}


void printStatus(const char* currentArgument, struct CompilerCommandStatusFlags status){
	const char* format = "Parsing argument: ";

	avLog(AV_DEBUG_INFO, "Parsing argument: ", currentArgument);

	if(checkError(status)){
		if(status.errors.inputFileAlreadySpecified){
			avLog(AV_INVALID_ARGUMENTS, "input file specified multiple times");
		}
		if(status.errors.inputStringTooLong){
			avLog(AV_INVALID_ARGUMENTS, "input string too long");
		}
		if(status.errors.invalidCommand){
			avLog(AV_INVALID_ARGUMENTS, "invalid command");
		}
		if(status.errors.multipleOutputFilesSpecified){
			avLog(AV_INVALID_ARGUMENTS, "multiple output files specified");
		}
		if(status.errors.multipleOutputTypesSpecified){
			avLog(AV_INVALID_ARGUMENTS, "multiple output types specified");
		}
		if(status.errors.unrecognizedFlag){
			avLog(AV_INVALID_ARGUMENTS, "unrecognized flag");
		}
	}

	if(checkWarning(status)){
		if(status.warnings.inputStringTooLong){
			avLog(AV_UNUSUAL_ARGUMENTS, "input string too long");
		}
		if(status.warnings.multipleVerbosityLevelsSpecified){
			avLog(AV_UNUSUAL_ARGUMENTS, "multiple verbosity levels specified");
		}
		if(status.warnings.multipleDebugStagesSpecified){
			avLog(AV_UNUSUAL_ARGUMENTS, "multiple debug stages specified");
		}
	}

	if(checkInfo(status)){
		if(status.infos.inputStringTruncated){
			avLog(AV_INFO, "input string truncated");
		}
		if(status.infos.inputValueOverridden){
			avLog(AV_INFO, "input value overridden");
		}
	}
}


bool32 parseCompilerArguments(const int32 argC, const char* argV[], struct CompilerCommand* commandOut){
	struct Arguments args = {
		.argC = argC,
		.argV = argV,
		.current = argV,
	};

	get(&args); // remove binary from command list

	struct ParseFlags flags = {0};
	struct CompilerCommand command = {
		.flags = {
			.outputType = COMPILER_OUTPUT_TYPE_DEFAULT,
		},
		.inputFile = {0},
		.outputFile = {"a.out"},
	};
	const char* currentArg = NULL;
	while(currentArg = peek(args)){
		struct CompilerCommandStatusFlags status = parseArgument(&args, &command, &flags);
		
		if(checkAnomaly(status)){
			printStatus(currentArg, status);
		}
	
		if(checkError(status)){
			return false;
		}
	}

	if(command.flags.printHelp){
		copyMem(&command, commandOut, sizeof(struct CompilerCommand));
		return true;
	}

	if(!flags.inputFileComplete){
		avLog(AV_INVALID_ARGUMENTS, "no input file specified");
		return false;
	}

	copyMem(&command, commandOut, sizeof(struct CompilerCommand));
	return true;
}

