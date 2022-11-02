SRC := src

INPUT_FILE_NAME  := 3DRENDER.cbl
OUTPUT_FILE_NAME := 3D-Renderer

DISPLAY_FILE := Output.txt

COMPILER := cobc

COMP_FLAGS := -x -Wall -Wcolumn-overflow -std=default -O

clean: 
	@echo Cleaning the executable...
	@rm   $(OUTPUT_FILE_NAME)
	@echo Finished!

run_program:
	@echo Deleting previous run...
	@rm   $(DISPLAY_FILE)
	@echo Printing displays to $(DISPLAY_FILE)...
	@./$(OUTPUT_FILE_NAME) >> $(DISPLAY_FILE)
	@echo Finished!

install:
	@echo Compiling 3D Renderer...
	@$(COMPILER) $(SRC)/$(INPUT_FILE_NAME) $(COMP_FLAGS) -o $(OUTPUT_FILE_NAME)  

