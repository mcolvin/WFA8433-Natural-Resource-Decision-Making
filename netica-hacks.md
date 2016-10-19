Triangular distribution equation
p (D | ) = Triangular3Dist(D,5,2.5,10) 
The node is D the mean is 5, min is 2.5 and max is 10



Discretizing a continuous variable 
Shortcut Notation:  If you want to create a list of evenly spaced values there are a few shortcut methods you can use. Each of the following special notations will expand into a list of numbers as described:
[b, e] / n will form a list beginning with b, ending with e, and having n intervals (so n + 1 numbers).
[b, e] + d will form a list beginning with b, ending with e, and each separated by d (except the last separation may be less if e – b is not evenly divisible by d).
[b, e] /L n will form a list beginning with b, ending with e, and divided logarithmically into n intervals (so n + 1 numbers).
[b, e] +% d will form a list beginning with b, ending with e, and each being d percent bigger than the previous (except the last may be less than d % bigger, if they don’t fit evenly).
Note:  If e is less than b then a decreasing list will be formed, but n and d should still be entered as positive numbers. The closing bracket may be replaced with a closing parenthesis if desired, to indicate excluding the endpoint e from the list formed. More than one of the above notations can be combined to form a longer list.
Examples:  Each line below is a complete example entry:
-3.2 0 1 1e4 infinity
/[0, 10/] // 10
[0, 10) + 1, [10, 20) + 2, [20, 30) + 3, 33, 37
[1e6, 1] /L 6
[200, 10] +% 15

Making a distribution from known data:

This is for parent node, but will apply to a child node equation.  
Example:  Say you did some research and found the distribution of a variable was normally distributed with mean 0.8 and standard deviation of 0.2.  In Netica, 
1.	make a new node to represent this variable and name the variable, lets call it “cost”.  
2.	State numbers:  Discretize the values observed under state numbers (see above)
3.	Equation:  type “p” netica will fill the rest of the left hand of the equation, then type “NormalDist(Cost,0.8, 0.2)” to the right of the equation.
4.	Under table in the menus select “equation to table”
5.	Compile the network
Note- when doing straight calculations you need to make sure the bins line up with all combinations if the input nodes are discrete

All Values:  If the equation is for a probabilistic node, its right-hand side must provide a probability for all the node’s possible values (so the name of the node must appear there at least once).  For example, if node Color (with states red, orange, yellow) has parent Temp (with states low, med, high), its equation could be:
p (Color | Temp) =
Temp == high ? (Color==yellow ? 1.0 : 0.0) :
Temp == med ? (Color==orange ? 1.0 : 0.0) :
Temp == low ? (Color==orange ? 0.2 :
Color==red ? 0.8 : 0.0) : 0
 
If you use the built-in distributions (such as NormalDist), the above is automatically taken care of.  One exception to the above is if a node is boolean.  Then only the probability for the true state need be given.  For example, if node It_Falls is boolean, its equation could be:
p (It_Falls | Weight, Size) =
Weight/Size > 10 ? 0.10 :
Weight/Size > 5 ? 0.03 :
0.01
 
Examples:  Here are some examples of equations:
X (Vel, dt, X0) = X0 + Vel * dt
 
p (X | Vel, dt, spread) = NormalDist (X, Vel*dt, spread)
 
Color (Taste) =
Taste==sour? blue: Taste==sweet? red:
Taste==salty? green: gray
 
p (Color | Taste) =
(Color==red && Taste==sweet) ? 0.7 : 0.1

