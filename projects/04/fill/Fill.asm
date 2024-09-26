// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(CHECK-KBD)
   // count = 8192
   @8192
   D=A
   @count
   M=D
   // current_address = SCREEN
   @SCREEN
   D=A
   @current_address
   M=D
   // if (KBD == 0) goto WHITE
   @KBD
   D=M
   @WHITE
   D;JEQ
(BLACK)
   // if (count == 0) goto CHECK-KBD
   @count
   D=M
   @CHECK-KBD
   D;JEQ
   // row[current_address] = -1
   @current_address
   D=M
   A=D
   M=-1
   // current_address = row[current_address] + 1
   @current_address
   D=M+1
   M=D
   // count = count - 1
   @count
   D=M-1
   M=D
   // goto BLACK
   @BLACK
   0;JMP
   // goto CHECK-KBD
   @CHECK-KBD
   0;JMP
(WHITE)
   // if (count == 0) goto CHECK-KBD
   @count
   D=M
   @CHECK-KBD
   D;JEQ
   // row[current_address] = 0
   @current_address
   D=M
   A=D
   M=0
   // current_address = row[current_address] + 1
   @current_address
   D=M+1
   M=D
   // count = count - 1
   @count
   D=M-1
   M=D
   // goto WHITE
   @WHITE
   0;JMP
   // goto CHECK-KBD
   @CHECK-KBD
   0;JMP