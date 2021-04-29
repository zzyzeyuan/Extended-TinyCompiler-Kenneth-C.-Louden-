cmake_minimum_required(VERSION 3.14)
project(TinyCompiler)

set(CMAKE_C_STANDARD 99)

include_directories(.)

add_executable(TinyCompiler
        main.c
        ANALYZE.C
        ANALYZE.H
        CGEN.C
        CGEN.H
        CODE.C
        CODE.H
        GLOBALS.H
        PARSE.C
        PARSE.H
        SCAN.C
        SCAN.H
        SYMTAB.C
        SYMTAB.H
        UTIL.C
        UTIL.H
        )