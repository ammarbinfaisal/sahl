#include <string>

#pragma once

class Const {
public:
    virtual void parse(std::istream &input) = 0;
    virtual std::string to_string() = 0;
};

class ConstString : public Const {
public:
    std::string value;

    void parse(std::istream &input);
    std::string to_string();
};

class ConstInt : public Const {
public:
    int64_t value;

    void parse(std::istream &input);
    std::string to_string();
};

class ConstFloat : public Const {
public:
    double value;

    void parse(std::istream &input);
    std::string to_string();
};

class ConstBool : public Const {
public:
    bool value;

    void parse(std::istream &input);
    std::string to_string();
};

class ConstChar : public Const {
public:
    char value;

    void parse(std::istream &input);
    std::string to_string();
};
