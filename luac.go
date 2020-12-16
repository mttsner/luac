package luac

import (
	"bytes"
	"encoding/binary"
	"github.com/yuin/gopher-lua"
)

// All of this should be removed when dealing with the final bytecode struct.
type opArgMode int

const (
	opArgModeN opArgMode = iota
	opArgModeU
	opArgModeR
	opArgModeK
)


type opType int

const (
	opTypeABC = iota
	opTypeABx
	opTypeASbx
)

type opProp struct {
	Name     string
	IsTest   bool
	SetRegA  bool
	ModeArgB opArgMode
	ModeArgC opArgMode
	Type     opType
}

var opProps = []opProp{
	opProp{"MOVE", false, true, opArgModeR, opArgModeN, opTypeABC},
	opProp{"LOADK", false, true, opArgModeK, opArgModeN, opTypeABx},
	opProp{"LOADBOOL", false, true, opArgModeU, opArgModeU, opTypeABC},
	opProp{"LOADNIL", false, true, opArgModeR, opArgModeN, opTypeABC},
	opProp{"GETUPVAL", false, true, opArgModeU, opArgModeN, opTypeABC},
	opProp{"GETGLOBAL", false, true, opArgModeK, opArgModeN, opTypeABx},
	opProp{"GETTABLE", false, true, opArgModeR, opArgModeK, opTypeABC},
	opProp{"SETGLOBAL", false, false, opArgModeK, opArgModeN, opTypeABx},
	opProp{"SETUPVAL", false, false, opArgModeU, opArgModeN, opTypeABC},
	opProp{"SETTABLE", false, false, opArgModeK, opArgModeK, opTypeABC},
	opProp{"NEWTABLE", false, true, opArgModeU, opArgModeU, opTypeABC},
	opProp{"SELF", false, true, opArgModeR, opArgModeK, opTypeABC},
	opProp{"ADD", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"SUB", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"MUL", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"DIV", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"MOD", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"POW", false, true, opArgModeK, opArgModeK, opTypeABC},
	opProp{"UNM", false, true, opArgModeR, opArgModeN, opTypeABC},
	opProp{"NOT", false, true, opArgModeR, opArgModeN, opTypeABC},
	opProp{"LEN", false, true, opArgModeR, opArgModeN, opTypeABC},
	opProp{"CONCAT", false, true, opArgModeR, opArgModeR, opTypeABC},
	opProp{"JMP", false, false, opArgModeR, opArgModeN, opTypeASbx},
	opProp{"EQ", true, false, opArgModeK, opArgModeK, opTypeABC},
	opProp{"LT", true, false, opArgModeK, opArgModeK, opTypeABC},
	opProp{"LE", true, false, opArgModeK, opArgModeK, opTypeABC},
	opProp{"TEST", true, true, opArgModeR, opArgModeU, opTypeABC},
	opProp{"TESTSET", true, true, opArgModeR, opArgModeU, opTypeABC},
	opProp{"CALL", false, true, opArgModeU, opArgModeU, opTypeABC},
	opProp{"TAILCALL", false, true, opArgModeU, opArgModeU, opTypeABC},
	opProp{"RETURN", false, false, opArgModeU, opArgModeN, opTypeABC},
	opProp{"FORLOOP", false, true, opArgModeR, opArgModeN, opTypeASbx},
	opProp{"FORPREP", false, true, opArgModeR, opArgModeN, opTypeASbx},
	opProp{"TFORLOOP", true, false, opArgModeN, opArgModeU, opTypeABC},
	opProp{"SETLIST", false, false, opArgModeU, opArgModeU, opTypeABC},
	opProp{"CLOSE", false, false, opArgModeN, opArgModeN, opTypeABC},
	opProp{"CLOSURE", false, true, opArgModeU, opArgModeN, opTypeABx},
	opProp{"VARARG", false, true, opArgModeU, opArgModeN, opTypeABC},
	opProp{"NOP", false, false, opArgModeR, opArgModeN, opTypeASbx},
}

type header struct {
	Signature     uint32
	Version       byte
	Format        byte
	Endianness    byte
	Int           byte
	SizeT         byte
	Instruction   byte
	LuaNumber 	  byte
	IntegralFlag  byte
}

func dumpString(buf *bytes.Buffer, str string) {
	binary.Write(buf, binary.LittleEndian, int32(len(str) + 1))
	buf.WriteString(str)
	buf.WriteByte(0)
}

func dumpInt(buf *bytes.Buffer, i int) {
	binary.Write(buf, binary.LittleEndian, int32(i))
}

func dumpNumber(buf *bytes.Buffer, num lua.LValue) {
	binary.Write(buf, binary.LittleEndian, lua.LVAsNumber(num))
}

func dumpHeader(buf *bytes.Buffer) {
	binary.Write(buf, binary.BigEndian, header{
		Signature:    0x1B4C7561,
		Version:      0x51,
		Format:       0,
		Endianness:   1,
		Int:          4,
		SizeT:        4,
		Instruction:  4,
		LuaNumber: 	  8,
		IntegralFlag: 0,
	})
}

func dumpFunction(buf *bytes.Buffer, f *lua.FunctionProto) {
	dumpString(buf, f.SourceName)
	dumpInt(buf, f.LineDefined) 	 
	dumpInt(buf, f.LastLineDefined)  
	buf.WriteByte(f.NumUpvalues) 	 
	buf.WriteByte(f.NumParameters) 
	buf.WriteByte(f.IsVarArg)
	buf.WriteByte(f.NumUsedRegisters)

	dumpCode(buf, f.Code)
	dumpConstants(buf, f.Constants)
	dumpPrototypes(buf, f.FunctionPrototypes)
	// Filling debug fields with zeros
	dumpInt(buf, 0)
	dumpInt(buf, 0)
	dumpInt(buf, 0)
}

// Everything here  should be done somewhere else
func createABC(op, a, b, c int) uint32 {
	return uint32(op)<<0 |
		   uint32(a)<<6  |
		   uint32(b)<<23 |
		   uint32(c)<<14
}

func createABx(op, a, bx int) uint32 {
	return uint32(op)<<0 |
		   uint32(a)<<6  |
		   uint32(bx)<<14
}

func createAsBx(op, a, sbx int) uint32 {
	return createABx(op, a, sbx + 131071)
}


func dumpCode(buf *bytes.Buffer, code []uint32) {
	dumpInt(buf, len(code))

	for _, inst := range code {
		// All of this is useless. I should be able to write the inst straight to the buffer.
		op     := int(inst >> 26)
		arga   := int(inst>>18) & 0xff
		argb   := int(inst & 0x1ff)
		argc   := int(inst>>9) & 0x1ff
		argbx  := int(inst & 0x3ffff)
		prop := &(opProps[op])

		switch prop.Type {
		case opTypeABC:
			inst = createABC(op, arga, argb, argc)
		case opTypeABx:
			inst = createABx(op, arga, argbx)
		case opTypeASbx:
			inst = createABx(op, arga, argbx)
		}
		binary.Write(buf, binary.LittleEndian, inst)
	}
}

func dumpPrototypes(buf *bytes.Buffer, protos []*lua.FunctionProto) {
	dumpInt(buf, len(protos))

	for _, proto := range protos {
		dumpFunction(buf, proto)
	}
}

func dumpConstants(buf *bytes.Buffer, consts []lua.LValue) {
	dumpInt(buf, len(consts))

	for _, cons := range consts {
		switch cons.Type() {
		case lua.LTNil:
			buf.WriteByte(0)
		case lua.LTBool:
			buf.WriteByte(1)
			binary.Write(buf, binary.LittleEndian, lua.LVAsBool(cons))
		case lua.LTNumber:
			buf.WriteByte(3)
			dumpNumber(buf, cons)
		case lua.LTString:
			buf.WriteByte(4)
			dumpString(buf, lua.LVAsString(cons))
		}
	}
}

// DumpLua turns FunctionProto into a luac file
func DumpLua(proto *lua.FunctionProto) []byte {
	buf := new(bytes.Buffer)
	dumpHeader(buf)
	dumpFunction(buf, proto)
	return buf.Bytes()
}