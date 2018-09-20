#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "mu-mips.h"

uint32_t previousInstruction;

/***************************************************************/
/* Extend Sign Function from 16-bits to 32-bits */
/***************************************************************/
uint32_t extend_sign(uint32_t immediate) {
	uint32_t data = (immediate & 0x0000FFFF);
	uint32_t mask = 0x00008000;
	if (mask & data) {
		data = data | 0xFFFF0000;
	}

	return data;
}

/***************************************************************/
/* Print out a list of commands available                                                                  */
/***************************************************************/
void help() {
	printf(
			"------------------------------------------------------------------\n\n");
	printf("\t**********MU-MIPS Help MENU**********\n\n");
	printf("sim\t-- simulate program to completion \n");
	printf("run <n>\t-- simulate program for <n> instructions\n");
	printf("rdump\t-- dump register values\n");
	printf("reset\t-- clears all registers/memory and re-loads the program\n");
	printf("input <reg> <val>\t-- set GPR <reg> to <val>\n");
	printf(
			"mdump <start> <stop>\t-- dump memory from <start> to <stop> address\n");
	printf("high <val>\t-- set the HI register to <val>\n");
	printf("low <val>\t-- set the LO register to <val>\n");
	printf("print\t-- print the program loaded into memory\n");
	printf("?\t-- display help menu\n");
	printf("quit\t-- exit the simulator\n\n");
	printf(
			"------------------------------------------------------------------\n\n");
}

/***************************************************************/
/* Read a 32-bit word from memory                                                                            */
/***************************************************************/
uint32_t mem_read_32(uint32_t address) {
	int i;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		if ((address >= MEM_REGIONS[i].begin)
				&& (address <= MEM_REGIONS[i].end)) {
			uint32_t offset = address - MEM_REGIONS[i].begin;
			return (MEM_REGIONS[i].mem[offset + 3] << 24)
					| (MEM_REGIONS[i].mem[offset + 2] << 16)
					| (MEM_REGIONS[i].mem[offset + 1] << 8)
					| (MEM_REGIONS[i].mem[offset + 0] << 0);
		}
	}
	return 0;
}

/***************************************************************/
/* Write a 32-bit word to memory                                                                                */
/***************************************************************/
void mem_write_32(uint32_t address, uint32_t value) {
	int i;
	uint32_t offset;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		if ((address >= MEM_REGIONS[i].begin)
				&& (address <= MEM_REGIONS[i].end)) {
			offset = address - MEM_REGIONS[i].begin;

			MEM_REGIONS[i].mem[offset + 3] = (value >> 24) & 0xFF;
			MEM_REGIONS[i].mem[offset + 2] = (value >> 16) & 0xFF;
			MEM_REGIONS[i].mem[offset + 1] = (value >> 8) & 0xFF;
			MEM_REGIONS[i].mem[offset + 0] = (value >> 0) & 0xFF;
		}
	}
}

/***************************************************************/
/* Execute one cycle                                                                                                              */
/***************************************************************/
void cycle() {
	handle_instruction();
	CURRENT_STATE = NEXT_STATE;
	INSTRUCTION_COUNT++;
}

/***************************************************************/
/* Simulate MIPS for n cycles                                                                                       */
/***************************************************************/
void run(int num_cycles) {

	if (RUN_FLAG == FALSE) {
		printf("Simulation Stopped\n\n");
		return;
	}

	printf("Running simulator for %d cycles...\n\n", num_cycles);
	int i;
	for (i = 0; i < num_cycles; i++) {
		if (RUN_FLAG == FALSE) {
			printf("Simulation Stopped.\n\n");
			break;
		}
		cycle();
	}
}

/***************************************************************/
/* simulate to completion                                                                                               */
/***************************************************************/
void runAll() {
	if (RUN_FLAG == FALSE) {
		printf("Simulation Stopped.\n\n");
		return;
	}

	printf("Simulation Started...\n\n");
	while (RUN_FLAG) {
		cycle();
	}
	printf("Simulation Finished.\n\n");
}

/***************************************************************/
/* Dump a word-aligned region of memory to the terminal                              */
/***************************************************************/
void mdump(uint32_t start, uint32_t stop) {
	uint32_t address;

	printf("-------------------------------------------------------------\n");
	printf("Memory content [0x%08x..0x%08x] :\n", start, stop);
	printf("-------------------------------------------------------------\n");
	printf("\t[Address in Hex (Dec) ]\t[Value]\n");
	for (address = start; address <= stop; address += 4) {
		printf("\t0x%08x (%d) :\t0x%08x\n", address, address,
				mem_read_32(address));
	}
	printf("\n");
}

/***************************************************************/
/* Dump current values of registers to the teminal                                              */
/***************************************************************/
void rdump() {
	int i;
	printf("-------------------------------------\n");
	printf("Dumping Register Content\n");
	printf("-------------------------------------\n");
	printf("# Instructions Executed\t: %u\n", INSTRUCTION_COUNT);
	printf("PC\t: 0x%08x\n", CURRENT_STATE.PC);
	printf("-------------------------------------\n");
	printf("[Register]\t[Value]\n");
	printf("-------------------------------------\n");
	for (i = 0; i < MIPS_REGS; i++) {
		printf("[R%d]\t: 0x%08x\n", i, CURRENT_STATE.REGS[i]);
	}
	printf("-------------------------------------\n");
	printf("[HI]\t: 0x%08x\n", CURRENT_STATE.HI);
	printf("[LO]\t: 0x%08x\n", CURRENT_STATE.LO);
	printf("-------------------------------------\n");
}

/***************************************************************/
/* Read a command from standard input.                                                               */
/***************************************************************/
void handle_command() {
	char buffer[20];
	uint32_t start, stop, cycles;
	uint32_t register_no;
	int register_value;
	int hi_reg_value, lo_reg_value;

	printf("MU-MIPS SIM:> ");

	if (scanf("%s", buffer) == EOF) {
		exit(0);
	}

	switch (buffer[0]) {
	case 'S':
	case 's':
		runAll();
		break;
	case 'M':
	case 'm':
		if (scanf("%x %x", &start, &stop) != 2) {
			break;
		}
		mdump(start, stop);
		break;
	case '?':
		help();
		break;
	case 'Q':
	case 'q':
		printf("**************************\n");
		printf("Exiting MU-MIPS! Good Bye...\n");
		printf("**************************\n");
		exit(0);
	case 'R':
	case 'r':
		if (buffer[1] == 'd' || buffer[1] == 'D') {
			rdump();
		} else if (buffer[1] == 'e' || buffer[1] == 'E') {
			reset();
		} else {
			if (scanf("%d", &cycles) != 1) {
				break;
			}
			run(cycles);
		}
		break;
	case 'I':
	case 'i':
		if (scanf("%u %i", &register_no, &register_value) != 2) {
			break;
		}
		CURRENT_STATE.REGS[register_no] = register_value;
		NEXT_STATE.REGS[register_no] = register_value;
		break;
	case 'H':
	case 'h':
		if (scanf("%i", &hi_reg_value) != 1) {
			break;
		}
		CURRENT_STATE.HI = hi_reg_value;
		NEXT_STATE.HI = hi_reg_value;
		break;
	case 'L':
	case 'l':
		if (scanf("%i", &lo_reg_value) != 1) {
			break;
		}
		CURRENT_STATE.LO = lo_reg_value;
		NEXT_STATE.LO = lo_reg_value;
		break;
	case 'P':
	case 'p':
		print_program();
		break;
	default:
		printf("Invalid Command.\n");
		break;
	}
}

/***************************************************************/
/* reset registers/memory and reload program                                                    */
/***************************************************************/
void reset() {
	int i;
	/*reset registers*/
	for (i = 0; i < MIPS_REGS; i++) {
		CURRENT_STATE.REGS[i] = 0;
	}
	CURRENT_STATE.HI = 0;
	CURRENT_STATE.LO = 0;

	for (i = 0; i < NUM_MEM_REGION; i++) {
		uint32_t region_size = MEM_REGIONS[i].end - MEM_REGIONS[i].begin + 1;
		memset(MEM_REGIONS[i].mem, 0, region_size);
	}

	/*load program*/
	load_program();

	/*reset PC*/
	INSTRUCTION_COUNT = 0;
	CURRENT_STATE.PC = MEM_TEXT_BEGIN;
	NEXT_STATE = CURRENT_STATE;
	RUN_FLAG = TRUE;
}

/***************************************************************/
/* Allocate and set memory to zero                                                                            */
/***************************************************************/
void init_memory() {
	int i;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		uint32_t region_size = MEM_REGIONS[i].end - MEM_REGIONS[i].begin + 1;
		MEM_REGIONS[i].mem = malloc(region_size);
		memset(MEM_REGIONS[i].mem, 0, region_size);
	}
}

/**************************************************************/
/* load program into memory                                                                                      */
/**************************************************************/
void load_program() {
	FILE * fp;
	int i, word;
	uint32_t address;

	/* Open program file. */
	fp = fopen(prog_file, "r");
	if (fp == NULL) {
		printf("Error: Can't open program file %s\n", prog_file);
		exit(-1);
	}

	/* Read in the program. */

	i = 0;
	while (fscanf(fp, "%x\n", &word) != EOF) {
		address = MEM_TEXT_BEGIN + i;
		mem_write_32(address, word);
		printf("writing 0x%08x into address 0x%08x (%d)\n", word, address,
				address);
		i += 4;
	}
	PROGRAM_SIZE = i / 4;
	printf("Program loaded into memory.\n%d words written into memory.\n\n",
			PROGRAM_SIZE);
	fclose(fp);
}

/************************************************************/
/* decode and execute instruction                                                                     */
/************************************************************/
void handle_instruction() {
	// Declarations
	uint32_t instruction;	// The code the program counter is pointing to
	uint32_t opcode;		// Operation Code
	uint32_t jump;			// The size of an instruction

	// Initializations
	instruction = mem_read_32(CURRENT_STATE.PC);
	opcode = instruction & 0xFC000000;
	jump = 0x4;

	switch (opcode) {
	case 0x00000000:
	{
		uint32_t rs = (instruction & 0x03E00000) >> 21;
		uint32_t rt = (instruction & 0x001F0000) >> 16;
		uint32_t rd = (instruction & 0x0000F800) >> 11;
		uint32_t shamt = (instruction & 0x000007C0) >> 6;
		uint32_t function = instruction & 0x0000003F;

		switch (function) {
		// ADD
		case 0x00000020:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					+ CURRENT_STATE.REGS[rt];
			break;

		// ADDU
		case 0x00000021:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					+ CURRENT_STATE.REGS[rt];
			break;

		// SUB
		case 0x00000022:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					- CURRENT_STATE.REGS[rt];
			break;

		// SUBU
		case 0x00000023:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					- CURRENT_STATE.REGS[rt];
			break;

		// MULT
		case 0x00000018:
		{
			if (previousInstruction == 0x0000012 || previousInstruction == 0x0000011) {
				printf("Undefined multiplication.");
				break;
			}

			uint64_t tempResult = CURRENT_STATE.REGS[rs]
					* CURRENT_STATE.REGS[rt];

			NEXT_STATE.LO = tempResult & 0xFFFFFFFF;
			NEXT_STATE.HI = tempResult >> 32;
			break;
		}

		// MULTU
		case 0x00000019: {
			if (previousInstruction == 0x0000012 || previousInstruction == 0x0000011) {
				printf("Undefined multiplication.");
				break;
			}

			uint64_t tempResult = CURRENT_STATE.REGS[rs]
					* CURRENT_STATE.REGS[rt];

			NEXT_STATE.LO = tempResult & 0xFFFFFFFF;
			NEXT_STATE.HI = tempResult >> 32;
			break;
		}

		// DIV
		case 0x0000001A:
			if (previousInstruction == 0x0000012 || previousInstruction == 0x0000011
					|| CURRENT_STATE.REGS[rt] == 0) {
				printf("Undefined division.");
				break;
			}

			NEXT_STATE.LO = CURRENT_STATE.REGS[rs] / CURRENT_STATE.REGS[rt];
			NEXT_STATE.HI = CURRENT_STATE.REGS[rs] % CURRENT_STATE.REGS[rt];
			break;

		// DIVU
		case 0x0000001B:
			if (previousInstruction == 0x0000012 || previousInstruction == 0x0000011
					|| CURRENT_STATE.REGS[rt] == 0) {
				printf("Undefined division.");
				break;
			}

			NEXT_STATE.LO = CURRENT_STATE.REGS[rs] / CURRENT_STATE.REGS[rt];
			NEXT_STATE.HI = CURRENT_STATE.REGS[rs] % CURRENT_STATE.REGS[rt];
			break;

		// AND
		case 0X00000024:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					& CURRENT_STATE.REGS[rt];
			break;

		// OR
		case 0X00000025:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					| CURRENT_STATE.REGS[rt];
			break;

		// XOR
		case 0X00000026:
			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs]
					^ CURRENT_STATE.REGS[rt];
			break;

		// NOR
		case 0x00000027:
			NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rs]
					| CURRENT_STATE.REGS[rt]);
			break;

		// SLT
		case 0x0000002A:
			if (CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt]) {
				NEXT_STATE.REGS[rd] = 0x00000001;
			}
			else {
				NEXT_STATE.REGS[rd] = 0x00000000;
			}
			break;

		// SLL
		case 0x00000000:
		{
			uint32_t tmp = CURRENT_STATE.REGS[rt] << shamt;
			NEXT_STATE.REGS[rd] = tmp;
			break;
		}

		// SRL
		case 0x00000002:
		{
			uint32_t tmp = CURRENT_STATE.REGS[rt] >> shamt;
			NEXT_STATE.REGS[rd] = tmp;
			break;
		}

		// SRA
		case 0x00000003:
		{
			uint32_t tmp;
			int x;
			uint32_t hiBit = CURRENT_STATE.REGS[rt] & 0x80000000;
			if (hiBit == 1) {
				tmp = CURRENT_STATE.REGS[rt];
				for (x = 0; x < shamt; x++) {
					tmp = ((tmp >> 1) | 0x80000000);
				}
			}
			else {
				tmp = CURRENT_STATE.REGS[rt] >> shamt;
			}
			NEXT_STATE.REGS[rd] = tmp;
			break;
		}

		// SYSCALL
		case 0x0000000C:
			NEXT_STATE.REGS[0] = 0xA;
			RUN_FLAG = FALSE;
			break;

		// JR
		case 0x00000008:
		{
			uint32_t tmp = CURRENT_STATE.REGS[rs];
			jump = tmp - CURRENT_STATE.PC;
			break;
		}

		// JALR
		case 0x00000009:
		{
			uint32_t tmp = CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 0x8;
			jump = tmp - CURRENT_STATE.PC;
			break;
		}

		// MTLO
		case 0x00000013:
			NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
			break;

		// MTHI
		case 0x0000011:
			NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
			break;

		// MFLO
		case 0x0000012:
			CURRENT_STATE.REGS[rd] = NEXT_STATE.LO;
			break;

		// MFHI
		case 0x0000010:
			CURRENT_STATE.REGS[rd] = NEXT_STATE.HI;
			break;
		}

		previousInstruction = function;
		break;
	}

	// J
	case 0x08000000:
	{
		uint32_t target = (0x03FFFFFF & instruction);
		uint32_t tmp = target << 2;
		uint32_t bits = (CURRENT_STATE.PC & 0xF0000000);
		jump = (bits | tmp) - CURRENT_STATE.PC;
		break;
	}

	// JAL
	case 0x0C000000:
	{
		uint32_t target = (0x03FFFFFF & instruction);
		uint32_t tmp = target << 2;
		uint32_t bits = (CURRENT_STATE.PC & 0xF0000000);
		NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 0x8;
		jump = (bits | tmp) - CURRENT_STATE.PC;
		break;
	}

	default: {

		uint32_t rs = (instruction & 0x03E00000) >> 21;
		uint32_t rt = (instruction & 0x001F0000) >> 16;
		uint32_t immediate = instruction & 0x0000FFFF;

		switch (opcode) {
		// ADDI
		case 0x20000000:
		{
			uint32_t result = extend_sign(immediate) + CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = result;
			break;
		}

		// ADDIU
		case 0x24000000:
		{
			uint32_t result = extend_sign(immediate) + CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = result;
			break;
		}

		// SB
		case 0xA000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			mem_write_32(eAddr, (CURRENT_STATE.REGS[rt]));
			break;
		}

		// SW
		case 0xAC000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			mem_write_32(eAddr, CURRENT_STATE.REGS[rt]);
			break;
		}

		// SH
		case 0xA4000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			mem_write_32(eAddr, CURRENT_STATE.REGS[rt]);
			break;
		}

		// LW
		case 0x8C000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = mem_read_32(eAddr);
			break;
		}

		// LB
		case 0x80000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = 0x0000000F | mem_read_32(eAddr);
			break;
		}

		// LH
		case 0x84000000:
		{
			uint32_t offset = extend_sign(immediate);
			uint32_t eAddr = offset + CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = 0x000000FF | mem_read_32(eAddr);
			break;

		}

		// ANDI
		case 0x30000000:
		{
			uint32_t tmp = (immediate & 0x0000FFFF) & CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = tmp;
			break;
		}

		// LUI
		case 0x3C000000:
		{
			NEXT_STATE.REGS[rt] = (immediate << 16);
			break;
		}

		// BEG
		case 0x10000000:
		{
			uint32_t target = extend_sign(immediate) << 2;
			if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
				jump = target;
			}
			break;
		}

		// BNE
		case 0x14000000:
		{
			uint32_t target = extend_sign(immediate) << 2;
			if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
				jump = target;
			}
			break;
		}

		// BLEZ
		case 0x18000000:
		{
			uint32_t target = extend_sign(immediate) << 2;
			if ((CURRENT_STATE.REGS[rs] & 0x80000000)
					|| (CURRENT_STATE.REGS[rt] == 0)) {
				jump = target;
			}
			break;
		}

		// BGTZ
		case 0x1C000000: {
			uint32_t target = extend_sign(immediate) << 2;
			if (!(CURRENT_STATE.REGS[rs] & 0x80000000)
					|| (CURRENT_STATE.REGS[rt] != 0)) {
				jump = target;
			}
			break;
		}

		// XORI
		case 0x38000000:
		{
			uint32_t tmp = (immediate & 0x0000FFFF) ^ CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = tmp;
			break;
		}

		// ORI
		case 0x34000000:
		{
			uint32_t tmp = (immediate & 0x0000FFFF) | CURRENT_STATE.REGS[rs];
			NEXT_STATE.REGS[rt] = tmp;
			break;
		}

		// REGIMM
		case 0x04000000: {
			switch (rt) {
			case 0x00000000: {
				uint32_t target = extend_sign(immediate) << 2;
				if ((CURRENT_STATE.REGS[rs] & 0x80000000)) {
					jump = target;
				}
			}
			case 0x00000001: {
				uint32_t target = extend_sign(immediate) << 2;
				if (!(CURRENT_STATE.REGS[rs] & 0x80000000)) {
					jump = target;
				}
			}
			}
			break;
		}
		}

	}

	}

	NEXT_STATE.PC = CURRENT_STATE.PC + jump;
}

/************************************************************/
/* Initialize Memory                                                                                                    */
/************************************************************/
void initialize() {
	init_memory();
	CURRENT_STATE.PC = MEM_TEXT_BEGIN;
	NEXT_STATE = CURRENT_STATE;
	RUN_FLAG = TRUE;
}

/************************************************************/
/* Print the program loaded into memory (in MIPS assembly format)    */
/************************************************************/
void print_program() {
	int i;
	uint32_t addr;

	for (i = 0; i < PROGRAM_SIZE; i++) {
		addr = MEM_TEXT_BEGIN + (i * 4);
		printf("\n[0x%x]\t", addr);
		print_instruction(addr);
	}
}

/************************************************************/
/* Print the instruction at given memory address (in MIPS assembly format)    */
/************************************************************/
void print_instruction(uint32_t addr) {
	// Declarations
	uint32_t instruction;
	uint32_t opcode;

	// Initializations
	instruction = mem_read_32(addr);
	opcode = (0xFC000000 & instruction);

	switch (opcode) {
	case 0x00000000: {
		uint32_t rs = (0x03E00000 & instruction) >> 21;
		uint32_t rt = (0x001F0000 & instruction) >> 16;
		uint32_t rd = (0x0000F800 & instruction) >> 11;
		uint32_t shamt = (0x000007C0 & instruction) >> 6;
		uint32_t function = (0x0000003F & instruction);

		switch (function) {
		// ADD
		case 0x00000020:
			printf("\nADD"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// ADDU
		case 0x00000021:
			printf("\nADDU"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// SUB
		case 0x00000022:
			printf("\nSUB"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// SUBU
		case 0x00000023:
			printf("\nSUBU"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// MULT
		case 0x00000018:
		{
			printf("\nMULT"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		// MULTU
		case 0x00000019:
		{
			printf("\nMULTU"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		// DIV
		case 0x0000001A:
			printf("\nDIV"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// DIVU
		case 0x0000001B:
			printf("\nDIVU"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// AND
		case 0X00000024:
			printf("\nAND"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// OR
		case 0X00000025:
			printf("\nOR"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// XOR
		case 0X00000026:
			printf("\nXOR"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// NOR
		case 0x00000027:
			printf("\nNOR"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// SLT
		case 0x0000002A:
			printf("\nSLTs"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// SLL
		case 0x00000000:
		{
			printf("\nSLL"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		// SRL
		case 0x00000002:
		{
			printf("\nSRL"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		// SRA
		case 0x00000003:
		{
			printf("\nSRA"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		// SYSCALL
		case 0x0000000C:
			printf("\nSYSCALL(exit)"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// JR
		case 0x00000008:
			printf("\nJR"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// JALR
		case 0x00000009:
			printf("\nJALR"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// MTLO
		case 0x00000013:
			printf("\nMTLO"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// MTHI
		case 0x0000011:
			printf("\nMTHI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// MFLO
		case 0x0000012:
			printf("\nMFLO"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;

		// MFHI
		case 0x0000010:
			printf("\nMFHI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    rd: %x"
					"\n    shamt: %x"
					"\n    function: %x\n", opcode, rs, rt, rd, shamt, function);
			break;
		}

		previousInstruction = function;
		break;
	}

	// JL
	case 0x08000000: {
		uint32_t target = (0x03FFFFFF & instruction);
		printf("\nJL"
				"\n    opcode: %x"
				"\n    target: %x\n", opcode, target);
		break;
	}

	// JAL
	case 0x0C000000: {
		uint32_t target = (0x03FFFFFF & instruction);
		printf("\nJAL"
				"\n    opcode: %x"
				"\n    target: %x\n", opcode, target);
		break;
	}

	default: {

		uint32_t rs = (0x03E00000 & instruction) >> 21;
		uint32_t rt = (0x001F0000 & instruction) >> 16;
		uint32_t immediate = (0x0000FFFF & instruction);

		switch (opcode) {
		// ADDI
		case 0x20000000:
		{
			printf("\nADDI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// ADDIU
		case 0x24000000:
		{
			printf("\nADDIU"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// SB
		case 0xA000000:
		{
			printf("\nSB"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// SW
		case 0xAC000000:
		{
			printf("\nSW"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// SH
		case 0xA4000000:
		{
			printf("\nSH"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// XORI
		case 0x38000000:
		{
			printf("\nSH"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// LW
		case 0x8C000000:
		{
			printf("\nLW"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// LB
		case 0x80000000:
		{
			printf("\nLB"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// LH
		case 0x84000000:
		{
			printf("\nLH"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;

		}

		// ANDI
		case 0x30000000:
		{
			printf("\nANDI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// LUI
		case 0x3C000000:
		{
			printf("\nLUI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// BEG
		case 0x10000000:
		{
			printf("\nBEG"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// BNE
		case 0x14000000:
		{
			printf("\nBNE"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// BLEZ
		case 0x18000000:
		{
			printf("\nBLEZ"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// BGTZ
		case 0x1C000000:
		{
			printf("\nBGTZ"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// ORI
		case 0x34000000:
		{
			printf("\nORI"
					"\n    opcode: %x"
					"\n    rs: %x"
					"\n    rt: %x"
					"\n    immediate: %x\n", opcode, rs, rt, immediate);
			break;
		}

		// REGIMM
		case 0x04000000:
		{
			switch (rt) {
			// BLTZ
			case 0x00000000:
			{
				printf("\nBLTZ"
						"\n    opcode: %x"
						"\n    rs: %x"
						"\n    rt: %x"
						"\n    immediate: %x\n", opcode, rs, rt, immediate);
				break;
			}

			// BGEZ
			case 0x00000001: {
				printf("\nBGEZ"
						"\n    opcode: %x"
						"\n    rs: %x"
						"\n    rt: %x"
						"\n    immediate: %x\n", opcode, rs, rt, immediate);
				break;
			}
			}
			break;
		}
		}

	}
	}
}

/***************************************************************/
/* main                                                                                                                                   */
/***************************************************************/
int main(int argc, char *argv[]) {
	printf("\n**************************\n");
	printf("Welcome to MU-MIPS SIM...\n");
	printf("**************************\n\n");

	if (argc < 2) {
		printf(
				"Error: You should provide input file.\nUsage: %s <input program> \n\n",
				argv[0]);
		exit(1);
	}

	strcpy(prog_file, argv[1]);
	initialize();
	load_program();
	help();
	while (1) {
		handle_command();
	}
	return 0;
}
