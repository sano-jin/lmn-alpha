#!/bin/bash
# count the lines of the program

cloc compiler/parser compiler/generator compiler/analyzer compiler/corelang compiler/*.ml runtime/vm runtime/pretty runtime/*.ml util $@


