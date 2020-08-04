# Automatic Code Synthesis
The third Scheme assignment is to complete the automatic code synthesis framework using the population evaluation function from Assignment 2 as your solution quality evaluator. The task is to write a function, which searches for the best program that performs a task given as pairs of the initial and the desired state of the maze. The ideal program would transform all the initial states to the desired states in the minimal number of steps.

##### Input
**(evolve <pairs> <threshold> <stack_size>)** where;
- **<pairs>** is a list of pairs of states, including the position and the orientation of the robot;
- **<threshold>** is lower bounds on the quality of the program in order to appear in the output;
- **<stack_size>** is the limit on the robot simulator stack size (see Assignment 2).

The inputs are exactly the same as in Assignment 2, with the exception of missing list of programs.

##### Output
**(<value> <program>)** where;
- **<value>** is the value of the output program in the sense of Assignment 2;
- **<program>** is the best program found for the given task;

The output is *NOT* the regular output of the function. Your function will be executed for a specific amount of time and then killed by the system. The output used by the system for evaluation will be the last line of the standard output produced by your program. Hence, you should always output the best found solution using display and then newline and flush-output function.

We do not put any hard restrictions on how your method works, but we suggest you to use genetic programming. In order to implement the genetic programming algorithm, you need to design a suitable initial population and implement the operations of selection, mutation and crossover.

__*Selection*__ selects the individuals that will participate in creating the next generation. Generally, the better individuals should have better chance to be selected. On the other hand, even the worse should be selected with non-zero probability to keep the population diverse and avoid local minima.

__*Mutation*__ is an operation on single individual of the genetic algorithm (program). It slightly randomly modifies the individual. For example, it can substitute one command/test for another, remove or add random command.

__*Crossover*__ is an operation on two selected individuals, which produces two new individuals as their combination. The basic selection could choose a sub-tree in each program and swap them.

The genetic programming evolves programs using the following high level algorithm:

1. create an initial population
2. evaluate all individuals in the population
3. if desired quality of the solution is reached then stop
4. select individuals participating on the new population
5. use mutation, crossover, and copy to create a new population
6. goto 2

In order to be able to evolve any program, it is important to design the mutation and crossover operations in a way that allows any program to be created from the initial population with non-zero probability.
