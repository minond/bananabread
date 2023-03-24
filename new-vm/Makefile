CC = clang++
CFLAGS = -Wall -std=c++20 -o vm
SRC = src/*.cpp

compile:
	$(CC) $(CFLAGS) $(SRC)

debug:
	$(CC) $(CFLAGS) $(SRC) -DDEBUG=1

main.cpp: compile
