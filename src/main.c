#include <types.h>
#include <utils.h>
#include <stdlib.h>

#include "cli/cli.h"

#include "compile/load.h"
#include "compile/tokenize.h"
#include "compile/parse.h"
#include "compile/analyse.h"
#include "compile/optimize.h"
#include "compile/generate.h"

#define AV_LOG_CATEGORY "AvLangCompiler"
#include <logging.h>

#define AV_VERSION(major, minor) (major << 16 | minor)

const char* disabledLogCategories[6] = {0};
uint32 disabledLogCategoryCount = 0;

AvResult disabledLogMessages[] = {};
uint32 disabledLogMessageCount = sizeof(disabledLogMessages) / sizeof(AvResult);

void configureLogSettings(AvLogSettings* logSettings, struct CompilerCommand command){
	switch(command.flags.verbosityLevel){
		case VERBOSITY_LEVEL_DEFAULT:
		case VERBOSITY_LEVEL_INFO:
			logSettings->level = AV_LOG_LEVEL_INFO;
			break;
		case VERBOSITY_LEVEL_DEBUG:
			logSettings->level = AV_LOG_LEVEL_DEBUG;
			break;
		case VERBOSITY_LEVEL_SUCCESS:
			logSettings->level = AV_LOG_LEVEL_ALL;
			break;
		case VERBOSITY_LEVEL_SILENT:
			logSettings->level = AV_LOG_LEVEL_NONE;
			break;
	}

	if(command.flags.stageDebugFlags.load){
		disabledLogCategories[disabledLogCategoryCount++] = LOAD_CATEGORY;
	}
	if(command.flags.stageDebugFlags.tokenize){
		disabledLogCategories[disabledLogCategoryCount++] = TOKENIZE_CATEGORY;
	}
	if(command.flags.stageDebugFlags.parse){
		disabledLogCategories[disabledLogCategoryCount++] = PARSE_CATEGORY;
	}
	if(command.flags.stageDebugFlags.analyze){
		disabledLogCategories[disabledLogCategoryCount++] = ANALYSE_CATEGORY;
	}
	if(command.flags.stageDebugFlags.optimize){
		disabledLogCategories[disabledLogCategoryCount++] = OPTIMIZE_CATEGORY;
	}
	if(command.flags.stageDebugFlags.generate){
		disabledLogCategories[disabledLogCategoryCount++] = GENERATE_CATEGORY;
	}
	logSettings->disabledCategoryCount = disabledLogCategoryCount;
	logSettings->disabledCategories = disabledLogCategories;
}

int main(const int argC, const char* argV[]){
	
	setProjectDetails("AvLangCompiler",  AV_VERSION(1, 0));

	AvLogSettings logSettings = avLogSettingsDefault;
	logSettings.printSuccess = true;
	logSettings.printCode = false;
	logSettings.printAssert = false;
	logSettings.printType = true;
	logSettings.printFunc = false;
	logSettings.printError = true;
	logSettings.printCategory = false;
	logSettings.colors = true;
	logSettings.disabledCategories = disabledLogCategories;
	logSettings.disabledCategoryCount = disabledLogCategoryCount;
	logSettings.disabledMessages = disabledLogMessages;
	logSettings.disabledMessageCount = disabledLogMessageCount;
	logSettings.validationLevel = AV_VALIDATION_LEVEL_WARNINGS_AND_ERRORS;
	logSettings.level = AV_LOG_LEVEL_ALL;
	setLogSettings(logSettings);

	struct CompilerCommand command = {0};
	if(!parseCompilerArguments(argC, argV, &command)){
		avLog(AV_ERROR_INFO, "failed to parse arguments");
		goto compilationTerminated;
	}

	configureLogSettings(&logSettings, command);
	setLogSettings(logSettings);

	if(command.flags.printHelp){
		printHelp();
		return 0;
	}

	avLog(AV_INFO, "Started Compilation");

	avLog(AV_SUCCESS, "parsed compiler arguments");

	struct StringBuffer inputFile = {0};
	if(!loadFile(command, &inputFile)){
		goto compilationTerminated;
	}
	avLog(AV_SUCCESS, "loaded file");

	struct TokenList tokens = {0};
	if(!tokenize(command, inputFile, &tokens)){
		goto compilationTerminated;
	}
	avLog(AV_SUCCESS, "tokenized text");

	struct AbstractSyntaxTree ast = {};
	if(!parse(command, tokens, &ast)){
		goto compilationTerminated;
	}
	avLog(AV_SUCCESS, "parsed tokens");

	avLog(AV_INFO, "compilation completed");

	return 0;
compilationTerminated:
	avLog(AV_ERROR_INFO, "compilation terminated");
	return -1;
}
