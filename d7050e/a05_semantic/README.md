# Assignment 5 - Use Semantic Analysis to Reject Erroneous Programs
In this assignment we will start working on the semantic analyser that will
essentially use the parsed AST data structure from assignment 2 and create meanings for this language.

## Type checking
To start with the semantic analyser we implement a type checker, this works similar to the interpreter
but instead of returning a value at each node of the tree we instead return the type of the value.
So for example if the tree has an addition binary operation the left sub tree evaluates to one type
and the same for the right subtree. The type checker has to make sure that both types are equal and they
implement the addition operation. Now let the left subtree evaluate to an integer and the right
to a boolean then the type checker will report a `TypeError` since the two types are not the same.
Now consider what happens if both types are a boolean and the operator is still an addition, then this
also reports a `TypeError` since boolean operands does not implement the plus operator.
