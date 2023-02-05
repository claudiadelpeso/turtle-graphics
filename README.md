# Turtle Graphics in Haskell

## Summary
This project implements a Turtle Graphics. Read more about it in: https://en.wikipedia.org/wiki/Logo_%28programming_language%29. 


##Files 
- Turtle.hs-> This file implements the abstact data type Instruction and Program. It also defines the data type Turtle which describes the turtle (position, color, angle...). It uses deep embedding to define all the actions a turtle can perform. 
- RunTurtle.hs -> This module contains the run function. It considers each possible action and gives the appropriate instructions. It uses the stateful monad to keep track of the current turtle state. 
The function takes a boolean argument that indicates if we want to run the graphical interface (True) or the text interface (False). The graphical option draws the actions the turtle performs on a new HGL window. 
The text option prints to screen the actions the turtle is performing, at what time and other additional information. Here each instruction is considered to take one time unit of time.
It includes functionality for infinite programs. However, if running both at the same time, it will only print to screen and not draw the actions. This is because the text function never ends so the graphical one never starts. 
-Example.hs -> Contains examples of programs you can execute if you do not want to write your own
- TurtleExtras.hs -> Defines extra functions a turtle can perform. 
- executable/Main.hs -> The run function is run in the main module. It defines the initial state of the turtle (which can be modified) and the program you want to execute i.e. the actions you want the turtle to perform. 

