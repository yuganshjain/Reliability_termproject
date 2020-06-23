# reliability-theory
Compute the probabiity of a compound system to fail.
https://en.wikipedia.org/wiki/Reliability_theory

## Goal
Given a system composed of several parallel or series subsystems, each of which succeeds which a given or unknown probability, compute the succeed rate of the full system.

It contains a **literal expression simplificator**. So you can enter a system with both hard-coded and literal reliability values, the result will always be printed nicely.

## How to use
Launch the program. Enter a system in the following syntax
### Syntax
#### schema
![reliability diagram](https://upload.wikimedia.org/wikipedia/commons/0/03/Reliability_block_diagram.png)

```
1 2/2/2 3 4
```

You can use parenthesis for more complex systems.

## Exemples
```
> 1/2
p_1 + p_2 - (p_1*p_2)
```

 ```
> 0.1/0.1
0.19
 ```
 
```
> a/b (c 0.9)/d 0.99
-0.99*(p_a*p_b*p_d) + -0.891*(p_a*p_b*p_c) + -0.891*(p_a*p_c*p_d) + -0.891*(p_b*p_c*p_d) + 0.891*(p_a*p_b*p_c*p_d) + 0.891*(p_a*p_c) + 0.891*(p_b*p_c) + 0.99*(p_a*p_d) + 0.99*(p_b*p_d)
```

