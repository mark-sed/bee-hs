# Bee syntax

* Case sensitive. Ebel instructions are case insensitive since, Ebel is case insensitive.
* Whitespace (space and tab) are ignored and newlines are ignored as well, but determine the end of a comment.
* Comment starts with `#` and ends with the end of a line or file/string. 
* Constructs/instructions are ended with `;`, exceptions are:
  * loop brackets (`{}`),
  * passes (`L:`).

`{}` - Looped instructions (LOOP will be placed at the position of `}`). `{` also implies a start of a new pass, by default it is a Words pass for Lines it needs to be specified (using `L` or `Lines`).

## Ebel instructions

Ebel instructions are part of Bee and can appear on their own. Ebel can be used as a replacement for Bee variables (`@` prefixed variables), which would be converted to Ebel instructions anyway. 

Ebel instructions can be prefixed with multiplier, that that allows to place the same instruction multiple times withou writing it by hand.

The following example deletes 43rd word or every line:
```
42 NOP; DEL;
```
This example could be replaced using `@i` (index) variable and condition.

## Bee variables

Bee scripts can use special variables to simplify writing the code. These variables represent Ebel instructions.

Every Bee variable has a `$` prefix.

* __`$`__ - Current word's value in it's real type. Same as in Ebe user defined expressions. Can be used only in expression.
* __`$i`__, __`$index`__ - Objects index, starting with 1.
* __`$col`__, __`$column`__ - Can be used in column based formats (like csv) to select specific column. The `$col` variable is an alias for `$i * 2 - 1`. Starts from 1.

## Conditions

```
cond ? true_part : false_part
```

Condition has syntax similar to ternary operator in C language, but does not require the else clause.

Example for condition that checks for string value and deletes it:
```
{"foo" ? del;}
```
Alternatively the else clause could be also written. 

The following example checks for the type at 3rd to be a NUMBER (could be also lowercase) and then swaps it with the word at 5th position:
```
2 NOP; NUMBER ? SWAP 2 : NOP;
```

When using logic operators, the equality can be only applied to a string value, but more conditionc can be connected using logic operators. The following example checks for 2 possible string values:
```
{"Hi" || "Hello" ? "Bye";}
```

## Expressions

Expressions can be only part of a condition and are the same as user defined expressions for Ebe.

`$` refers to the matched value (in its actual type and not a string). 

Example of modifying all number values:
```
{ NUMBER ? ($ + 42) * 3; }
```

## Passes
`L:`, `Lines:` - Lines pass.

`W:`, `Words:` - Words pass.

Note that Expression pass is generated from conditional statements and expressions.
