# Assignment 4 - Implement an Interpreter from the AST
In this assignment we will create an interpreter using Structural Operational Semantics (SOS).
I will use the previous parser from assignment 2 to implement this interpreter. As an extra assignment
I will also try to implement operator precedence by using the precedence climbing algorithm.
## Structural Operational Semantics
For structural operational semantics this notation is used
![equation](http://www.sciweavers.org/tex2img.php?eq=%5Cleft%3Ce%2C%5Csigma%5Cright%3E%20%5CDownarrow%20n%24&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0)
where `e` is a given expression or value, `sigma` is the current environment and `n` is