#ifndef __COMPILER_STATE__
#define __COMPILER_STATE__

#include <types.h>

#define MAX_FILE_NAME_LENGTH (4098)
#define NULL_TERMINATOR_LENGTH (1)

#define LOAD_CATEGORY "load"
#define TOKENIZE_CATEGORY "tokenize"
#define PARSE_CATEGORY "parse"
#define ANALYSE_CATEGORY "analyse"
#define OPTIMIZE_CATEGORY "optimize"
#define GENERATE_CATEGORY "generate"

struct CompilerStateFlags{
	uint32 errorOccured : 1;
	uint32 tokenizing : 1;
	uint32 parsing : 1;
	uint32 analysing : 1;
	uint32 optimising : 1;
	uint32 generating : 1;
};

struct CompilerStateErrorFlags{
	uint32 ioError : 1;
	uint32 fileNotFound : 1;
	uint32 readIncomplete : 1;
	uint32 writeIncomplete : 1;
};

enum CompilerOutputType {
	COMPILER_OUTPUT_TYPE_DEFAULT = 0,
	COMPILER_OUTPUT_TYPE_OBJECT,
	COMPILER_OUTPUT_TYPE_ASSEMBLY,
	COMPILER_OUTPUT_TYPE_IR,
};

struct StageDebugFlags {
	uint32 load: 1;
	uint32 tokenize: 1;
	uint32 parse: 1;
	uint32 analyze: 1;
	uint32 optimize: 1;
	uint32 generate: 1;
};

enum VerbosityLevel {
	VERBOSITY_LEVEL_DEFAULT = 0,
	VERBOSITY_LEVEL_SILENT,
	VERBOSITY_LEVEL_INFO,
	VERBOSITY_LEVEL_SUCCESS,
	VERBOSITY_LEVEL_DEBUG,
};

struct CompilerStateCommandFlags{
	enum CompilerOutputType outputType;
	enum VerbosityLevel verbosityLevel;
	bool8 printHelp;
	struct StageDebugFlags stageDebugFlags;
};

struct CompilerCommandErrorFlags{
	uint32 invalidCommand : 1;
	uint32 unrecognizedFlag : 1;
	uint32 inputStringTooLong : 1;
	uint32 inputFileAlreadySpecified : 1;
	uint32 multipleOutputTypesSpecified : 1;
	uint32 multipleOutputFilesSpecified : 1;
};

struct CompilerCommandWarningFlags {

	uint32 inputStringTooLong : 1;
	uint32 multipleVerbosityLevelsSpecified: 1;
	uint32 multipleDebugStagesSpecified: 1;
};

struct CompilerCommandInfoFlags {
	uint32 inputStringTruncated : 1;
	uint32 inputValueOverridden : 1;
};

struct CompilerCommandStatusFlags {
	struct CompilerCommandErrorFlags errors;
	struct CompilerCommandWarningFlags warnings;
	struct CompilerCommandInfoFlags infos;
};

struct CompilerCommand{
	struct CompilerStateCommandFlags flags;
	const char outputFile[MAX_FILE_NAME_LENGTH + NULL_TERMINATOR_LENGTH];
	const char inputFile[MAX_FILE_NAME_LENGTH + NULL_TERMINATOR_LENGTH];
};



struct CompilerState {
	struct CompilerStateFlags flags;
	struct CompilerStateErrorFlags errors;
	struct CompilerCommand command;
};

bool32 checkAnomaly(struct CompilerCommandStatusFlags status);

bool32 checkError(struct CompilerCommandStatusFlags status);
bool32 checkWarning(struct CompilerCommandStatusFlags status);
bool32 checkInfo(struct CompilerCommandStatusFlags status);

#endif//__COMPILER_STATE__

