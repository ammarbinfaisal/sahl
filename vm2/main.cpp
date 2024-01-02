#include "code.h"
#include <fstream>

int main() {
    std::ifstream input("exe.bin");
    parse_binary(input);
}