# Tiny-Computer-Simulator

## What is it?
The tiny computer simulator is Racket program that simulates the Von Neumann architecture. The simulator assembles and executes assembly language programs fed into it! Here, I used a recursive approach to craft this project while making use of the built-in struct data structure in Racket.

## What is in the program?
- I constructed a framework for a RAM along with procedures to perform read and write operations on these RAMs.
- I built a framework for a CPU comprising registers such as the accumulator and the program counter as well as wrote procedures to configure the CPU, look up values in the registers, assemble programs and produce the expected output from the program.
- I wrote procedures for each assembly language instruction to be accepted by my Tiny Computer and mapped them with approppriate binary OPCODES.
- I also wrote assembly language programs such as one the encrypts an input given a key and another that reverses its given input to test my Tiny Computer. 
- The code is heavily commented with explanations, details about the design and test cases. You can also find a test script at the end of the program!

![hacker-hacker-man](https://user-images.githubusercontent.com/50711847/174885749-d36d2507-f399-4c46-9de8-a0e056638b04.gif) ![computer-family-computer](https://user-images.githubusercontent.com/50711847/174885868-9c213bfe-4a4a-4b42-84f2-0998775fbb71.gif)

