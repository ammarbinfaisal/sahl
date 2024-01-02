#include "consts.h"

#include <iostream>

void ConstString::parse(std::istream &input) {
    uint64_t length;
    input.read(reinterpret_cast<char *>(&length), sizeof(uint64_t));
    value.resize(length);
    input.read(&value[0], length);
}

void ConstInt::parse(std::istream &input) {
    input.read(reinterpret_cast<char *>(&value), sizeof(int64_t));
}

void ConstFloat::parse(std::istream &input) {
    input.read(reinterpret_cast<char *>(&value), sizeof(double));
}

void ConstBool::parse(std::istream &input) {
    input.read(reinterpret_cast<char *>(&value), sizeof(bool));
}

void ConstChar::parse(std::istream &input) { input.read(&value, sizeof(char)); }

std::string ConstString::to_string() { return "\"" + value + "\""; }

std::string ConstInt::to_string() { return std::to_string(value); }

std::string ConstFloat::to_string() { return std::to_string(value); }

std::string ConstBool::to_string() { return value ? "true" : "false"; }

std::string ConstChar::to_string() { return "'" + std::string(1, value) + "'"; }