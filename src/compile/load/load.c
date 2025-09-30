#include "../load.h"

#include <utils.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#define AV_LOG_CATEGORY LOAD_CATEGORY
#include <logging.h>

// this function loads the contents of the file into memory, and replaces CRLF with LF
bool32 loadFile(struct CompilerCommand command, struct StringBuffer* bufferOut){

	const int file = open(command.inputFile, O_RDONLY);
	if(file == -1){
		avLog(AV_IO_ERROR, strerror(errno));
		return false;
	}

	struct stat st;
	if(stat(command.inputFile, &st)==-1){
		avLog(AV_IO_ERROR, strerror(errno));
		return false;
	}
	
	const uint64 size = st.st_size;
	if(size == 0){
		avLog(AV_IO_ERROR, "File of size 0");
		return false;
	}
	uint64 allocSize = size+NULL_TERMINATOR_LENGTH;
	char* rawData = (char*) memset(malloc(allocSize), 0, allocSize);
	if(rawData == NULL){
		avLog(AV_IO_ERROR, "Out of memory");
		return false;
	}

	uint64 readBytes = read(file, rawData, size);
	if(readBytes == -1){
		avLog(AV_IO_ERROR, "Error reading file");
		return false;
	}

	if(readBytes != size){
		avLog(AV_IO_ERROR, "Could not read file in one go");
		free(rawData);
		return false;
	}

	char* data = (char*) memset(malloc(allocSize), 0, allocSize);
	if(data == NULL){
		avLog(AV_IO_ERROR, "Out of memory");
		return false;
	}

	uint64 writeIndex = 0;
	uint64 readIndex = 0;

	while(readIndex < size){
		const char c = rawData[readIndex++];
		if(c == '\r'){
			continue;
		}
		data[writeIndex++] = c;
	}
	data[writeIndex] = '\0';

	struct StringBuffer buffer = {
		.data = (char*) data,
		.length = writeIndex
	};

	copyMem(&buffer, bufferOut, sizeof(struct StringBuffer));
	
	avLog(AV_DEBUG, "loaded: \n\n",buffer.data, "\n\n");

	return true;
}