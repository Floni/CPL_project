# Viw

## Extra Tests

In my code I made a file and class for each of the major categories of commands.
The tests for those commands are also divided in the same way into different
test files. I added some edge cases and more complicated tests for the base
commands and did the same for all non-base commands.

## Extensibility

If you would want to add a new movement command then you have to do the following:
first you would need to add the mapping of the key to the new command to the commandMap
in Viw.scala, then you would need to add the class itself. In this case you would have
to add a new class NewMovementCommand(..) to the MoveCommand file that extends the
abstract MoveCommand class. The class only needs to implement the method getNewPos which
returns a new position from a given position (the effect of the movement command).

All other new commands require the same amount of changes, add it to the commandMap)
and add a new subclass in the appropriate command category file. Some special commands
that for example do something specific when you repeat the command twice (like dd)
require some more changes but these are always only necessary in the Viw file itself.

## Custom Extension

I have not added any custom extensions. The project contains all of the functionalities
from the assigment except for visual and find.

## Feedback on the Project 

After working on this thing for such a long time you're allowed to vent in this
section. Tell me what you learned or what you would've liked to learn instead,
etc. This does not count toward your grades at all (positive nor negative).

The project was quite enjoyable, scala is a fun way to combine OOP and FP. The
assignment itself to implement some vim features is also fun especially when you
have some vim experience. For me the biggest challenge was to keep a somewhat clean
implementation when adding new commands from the true vim text modification part.
While the base commands were relatively easy to implement cleanly these extra commands
had a lot of conflicting requirements which made it hard to have a very clean implementation.

It was good to know what you had to do to obtain a passing grade but it would 
also have been good to know how much you had to do to obtain a higher grade.
For example if you would want close to a 20 then you would need to have implemented
the whole assignment flawlessly in a somewhat clean way.