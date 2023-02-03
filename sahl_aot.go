package main

import (
	"fmt"
	"math/rand"
	"os"
)

// convert sahl bytecode to assembly

const ADD = 0
const SUB = 1
const MUL = 2
const DIV = 3
const MOD = 4
const NEG = 5
const NOT = 6
const AND = 7
const OR = 8
const EQUAL = 9
const NOT_EQUAL = 10
const LESS = 11
const LESS_EQUAL = 12
const GREATER = 13
const GREATER_EQUAL = 14
const TRUE = 15
const FALSE = 16
const JUMP = 17
const JUMP_IF_FALSE = 18
const STORE = 19
const INDEX = 20
const APPEND = 21
const LENGTH = 22
const LIST = 23
const CONST_U64 = 24
const CONST_U32 = 25
const CONST_U8 = 26
const STRING = 27
const DEF_LOCAL = 28
const GET_LOCAL = 29
const ASSIGN = 30
const CALL = 31
const RETURN = 32
const PRINT = 33
const POP = 34
const MAKE_TUPLE = 36

type Code struct {
	Strings   []string
	Functions []Function
	Start     int
}

type Function struct {
	Instructions []uint8
}

func ReadInt32(buffer []uint8, offset int) int {
	return int(buffer[offset]) | int(buffer[offset+1])<<8 | int(buffer[offset+2])<<16 | int(buffer[offset+3])<<24
}

func ReadInt64(buffer []uint8, offset int) int64 {
	return int64(buffer[offset]) | int64(buffer[offset+1])<<8 | int64(buffer[offset+2])<<16 | int64(buffer[offset+3])<<24 | int64(buffer[offset+4])<<32 | int64(buffer[offset+5])<<40 | int64(buffer[offset+6])<<48 | int64(buffer[offset+7])<<56
}

func ReadCode(file string) *Code {
	contents, err := os.ReadFile(file)

	if err != nil {
		println("Error: Could not open file " + file)
		os.Exit(1)
	}

	functions := make([]Function, 0)
	strings := make([]string, 0)

	// first 4 bytes are the start function
	start := ReadInt32(contents, 0)
	// next 4 bytes are the number of strings
	string_count := ReadInt32(contents, 4)

	offset := 8
	for i := 0; i < string_count; i++ {
		strlength := ReadInt32(contents, offset)
		offset += 4
		str := string(contents[offset : offset+strlength])
		offset += strlength
		strings = append(strings, str)
	}

	func_count := ReadInt32(contents, offset)
	offset += 4

	for i := 0; i < func_count; i++ {
		func_length := ReadInt32(contents, offset)
		offset += 4
		functions = append(functions, Function{Instructions: contents[offset : offset+func_length]})
		offset += func_length
	}

	return &Code{Strings: strings, Functions: functions, Start: start}
}

func PrintOpcode(code []byte, i int) int {
	switch code[i] {
	case ADD:
		fmt.Println("Add")
	case SUB:
		fmt.Println("Sub")
	case MUL:
		fmt.Println("Mul")
	case DIV:
		fmt.Println("Div")
	case MOD:
		fmt.Println("Mod")
	case NEG:
		fmt.Println("Neg")
	case NOT:
		fmt.Println("Not")
	case AND:
		fmt.Println("And")
	case OR:
		fmt.Println("Or")
	case EQUAL:
		fmt.Println("Equal")
	case NOT_EQUAL:
		fmt.Println("NotEqual")
	case LESS:
		fmt.Println("Less")
	case LESS_EQUAL:
		fmt.Println("LessEqual")
	case GREATER:
		fmt.Println("Greater")
	case GREATER_EQUAL:
		fmt.Println("GreaterEqual")
	case TRUE:
		fmt.Println("True")
	case FALSE:
		fmt.Println("False")
	case JUMP:
		// u64 code
		fmt.Printf("Jump %d\n", ReadInt32(code, i+1))
		i += 4
	case JUMP_IF_FALSE:
		// u64 code
		fmt.Printf("JumpIfFalse %d\n", ReadInt32(code, i+1))
		i += 4
	case STORE:
		// u8 index
		fmt.Println("Store")
	case INDEX:
		fmt.Println("Index")
	case CONST_U32:
		fmt.Printf("ConstU32 %d\n", ReadInt32(code, i+1))
		i += 4
	case CONST_U64:
		fmt.Printf("ConstU64 %d\n", ReadInt64(code, i+1))
		i += 8
	case CONST_U8:
		fmt.Printf("ConstU8 %d\n", code[i+1])
		i += 1
	case LIST:
		fmt.Printf("List %d\n", ReadInt32(code, i+1))
		i += 4
	case STRING:
		stridx := ReadInt32(code, i+1)
		i += 5
		fmt.Printf("string at index %d\n", stridx)
	case APPEND:
		fmt.Println("Append")
	case DEF_LOCAL:
		fmt.Printf("DefLocal %d\n", ReadInt32(code, i+1))
		i += 4
	case GET_LOCAL:
		fmt.Printf("GetLocal %d\n", ReadInt32(code, i+1))
		i += 4
	case ASSIGN:
		fmt.Printf("Assign %d\n", ReadInt32(code, i+1))
		i += 4
	case LENGTH:
		fmt.Println("Length")
	case CALL:
		fmt.Printf("Call \t fn: %d \t arg count: %d \n", ReadInt32(code, i+1), ReadInt32(code, i+5))
		i += 8
	case RETURN:
		fmt.Println("Return")
	case PRINT:
		fmt.Println("Print")
	case POP:
		fmt.Println("Pop")
	default:
		fmt.Printf("Unknown opcode %d\n", code[i])
	}
	return i
}

func disassemble(code []byte) {
	i := 0
	for i < len(code) {
		fmt.Printf("%5d\t", i)
		i = PrintOpcode(code, i)
		i++
	}
}

// ------------------------------
// ---- Compiler
// ------------------------------

type Compiler struct {
	lines []string
	code  *Code
}

func (c *Compiler) WriteData(strings []string) {
	// add strings
	c.AddLine("section .data", false)
	for i := 0; i < len(strings); i++ {
		c.AddLine(fmt.Sprintf("string%d: db \"%s\", 0", i, strings[i]), true)
	}
	c.AddLine(fmt.Sprintf("string%d: db \"%%d\", 10, 0", len(strings)), true)
}

// x86-64 registers
var Registers = []string{"rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"}

type ValueType uint

const (
	VALUE_REG ValueType = iota
	VALUE_CONST
	VALUE_STACK
)

type Value struct {
	Type  ValueType
	Value string
}

func rand_str() string {
	chars := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	b := make([]byte, 8)
	for i := range b {
		b[i] = chars[rand.Intn(len(chars))]
	}
	return string(b)
}

func (c *Compiler) AddLine(line string, indent bool) {
	if indent {
		c.lines = append(c.lines, "\t")
	} else {
		c.lines = append(c.lines, "")
	}
	c.lines[len(c.lines)-1] += line
}

// convert bytecodes to assembly
func (c *Compiler) CompileFunc(fn *Function, name string) {
	stack := make([]Value, 0)

	_unused := func() string {
		for _, reg := range Registers {
			found := false
			for _, r := range stack {
				if r.Value == reg {
					found = true
					break
				}
			}
			if !found {
				return reg
			}
		}
		panic("no unused registers")
	}

	stack_size := 0

	c.AddLine(name+": ", false)
	c.AddLine("push rbp", true)
	c.AddLine("mov rbp, rsp", true)
	c.AddLine("", false) // space for stack size

	idx_sub := len(c.lines) - 1

	for i := 0; i < len(fn.Instructions); i++ {
		instr := fn.Instructions[i]

		PrintOpcode(fn.Instructions, i)

		switch instr {
		case ADD:
			r1 := stack[len(stack)-1]
			r2 := stack[len(stack)-2]
			if r1.Type == VALUE_CONST && r2.Type == VALUE_CONST {
				r := _unused()
				c.AddLine(fmt.Sprintf("mov %s, %s", r, r1.Value), true)
				c.AddLine(fmt.Sprintf("add %s, %s", r, r2.Value), true)
				stack = stack[:len(stack)-2]
				stack = append(stack, Value{VALUE_REG, r})
			} else if r1.Type == VALUE_CONST || r2.Type == VALUE_CONST {
				r := _unused()
				if r1.Type == VALUE_CONST {
					c.AddLine(fmt.Sprintf("mov %s, %s", r, r1.Value), true)
					c.AddLine(fmt.Sprintf("add %s, %s", r, r2.Value), true)
				} else {
					c.AddLine(fmt.Sprintf("mov %s, %s", r, r2.Value), true)
					c.AddLine(fmt.Sprintf("add %s, %s", r, r1.Value), true)
				}
				stack = stack[:len(stack)-2]
				stack = append(stack, Value{VALUE_REG, r})
			} else if r1.Type == VALUE_REG && r2.Type == VALUE_REG {
				c.AddLine(fmt.Sprintf("add %s, %s", r1.Value, r2.Value), true)
				stack = stack[:len(stack)-2]
				stack = append(stack, Value{VALUE_REG, r1.Value})
			} else {
				panic("invalid add")
			}
		case CONST_U64:
			stack = append(stack, Value{VALUE_CONST, fmt.Sprintf("%d", ReadInt64(fn.Instructions, i+1))})
			i += 8
		case PRINT:
			r := stack[len(stack)-1]
			c.AddLine(fmt.Sprintf("mov rdi, string%d", len(c.code.Strings)), true)
			c.AddLine(fmt.Sprintf("mov rsi, %s", r.Value), true)
			c.AddLine("mov rax, 0", true)
			c.AddLine("call printf", true)
		case RETURN:
			r := stack[len(stack)-1]
			if r.Type == VALUE_CONST || r.Type == VALUE_REG {
				c.AddLine(fmt.Sprintf("mov rax, %s", r.Value), true)
			} else {
				panic("invalid return")
			}
			c.AddLine(fmt.Sprintf("jmp %s_ret", name), true)
		default:
			fmt.Printf("Cannot compile opcode \t")
			PrintOpcode(fn.Instructions, i)
			fmt.Println()
			os.Exit(1)
		}
	}

	c.lines[idx_sub] = fmt.Sprintf("\tsub rsp, %d", stack_size)

	c.AddLine(fmt.Sprintf("%s_ret:", name), false)
	c.AddLine("pop rbp", true)
	c.AddLine(fmt.Sprintf("sub rsp, %d", stack_size), true)
	c.AddLine("ret", true)
}

func (c *Compiler) Compile(code *Code) {
	c.WriteData(code.Strings)
	c.AddLine("section .text", false)
	c.AddLine("extern printf", false)
	c.AddLine("global main", false)

	fn_name := rand_str()
	c.CompileFunc(&code.Functions[code.Start], fn_name)
	c.AddLine("main:", false)
	c.AddLine(fmt.Sprintf("call %s", fn_name), true)
	// exit
	c.AddLine("mov rax, 60", true)
	c.AddLine("mov rdi, 0", true)
	c.AddLine("syscall", true)
}

func (c *Compiler) Write(file string) {
	f, err := os.Create(file)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	for _, line := range c.lines {
		f.WriteString(line + "\n")
	}
}

func main() {
	args := os.Args
	if len(args) < 2 {
		println("usage: ./sahl_aot <file>")
		return
	}
	file := args[1]
	code := ReadCode(file)
	for i := 0; i < len(code.Functions); i++ {
		if i == code.Start {
			fmt.Println("Start function")
		} else {
			fmt.Printf("Function %d\n", i)
		}
		disassemble(code.Functions[i].Instructions)
	}
	compiler := Compiler{
		code:  code,
		lines: make([]string, 0),
	}
	compiler.Compile(code)
	compiler.Write("exe.asm")
}
