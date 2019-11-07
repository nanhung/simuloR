CC = gcc
mod_SOURCES_C = $(wildcard mod/*.c)
mod_TARGET = mod.so

all: $(mod_TARGET)

$(mod_TARGET): $(mod_SOURCES_C)
	$(CC) -o $(mod_TARGET) $(mod_SOURCES_C)

