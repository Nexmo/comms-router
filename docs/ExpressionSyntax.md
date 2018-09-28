# Comms Router Expressions

## Introduction
Comms Router uses a custom expression syntax for binding Agents to Queues and filtering Tasks into different queues in a plan configuration. Expressions are composed of constants, functions, as well as logical and comparison operators.

For example, a Queue expression looks like:

```
"HAS(#{skills},'electronics') && IN(#{language},['en','ru','es'])"
```

And a Plan filter expression looks like:

```
"#{customer_value} == 'Gold' && #{type} == 'ticket'"
```

## Expression Structure 
Expressions are made up of simple comparisons of constants and/or JSON keys, grouped together by parenthesis functions, and logical operators.

Examples:
```
1 == 1
0 != 1
'alice' != 'bob'
#{key} > 1
#{param} != 'bob'
(#{condition1} == true) || (#{condition2} == true)
(#{language} == 'en' || #{language} == 'fr') && #{skill_rating} >= 5.1
IN(#{language}, ['en', 'es'])
HAS(#{computer_languages}, 'java')
CONTAINS(#{lvalue}, 'market')
```

### Constants
There are 3 types of constants supported:
* Strings, represented as single quoted blocks of text. 'string'.
* Numbers, represented as integers or floating point numbers.  1, 1.0, 1.00, 3.141529
* Booleans, represented as unquoted true or false.

Constants can be the left or right values of a comparison operator.

### Attributes
Attributes must be enclosed by a pound sign and open brace __#{__ and a closed brace __}__.

An expression with attribute could be:
```
#{a} == #{b}
#{temperature} < 100
#{name} != 'john'
```

### References

References to JSON object keys can also exist in the expression--for example, in a Plan document when evaluating Task attributes. These are resolved against the JSON document being evaluated and the value in the document is substituted. If the document does not contain the requested key, it resolves to NULL.

Given the Task attributes:

```
{
    "string_attribute": "foo",
    "string_array": ["a","b","c"],
    "int_array": [1,2,3,4,5],
    "int_const": 123
}
```

The following are valid keys in a Plan configuration, and would resolve as follows:
* __string_attribute__ - a string that resolves to __"foo"__
* __string_array__ - an array that resolves to __["a","b","c"]__
* __int_array__ - an array that resolves to __[1,2,3,4,5]__
* __int_array__ - an integer that resolves to __123__

### Comparison Operators
Comparison operators compare two constants and return true or false. Comparisons between different types should be avoided. Scalar operators can compare two scaler values (i.e. not arrays). Array operators are not supported. To use array comparisons you can use one of the two predefined functions HAS and IN.

Valid scalar operators are:

* __\>__  greater than
* __\>=__ greater than or equal to
* __=__ equals
* __!=__ does not equal
* __<=__ less than or equal to
* __<__  less than

### Comparison Functions
Valid functions are:

* __HAS(#{array}, 'value')__ - does array in the first argument contain the value in the second argument
* __IN('value', #{array})__ - does the scalar value on the first argument exists in the array on the second argument.
* __CONTAINS(#{value1}, #{value2})__ - does the scalar value on the left contain the scalar value on the right.

### Logical Operators
Compares the results of sub-expressions to the left and right and return true or false based on the operand. Parenthesis can be used to group sub-expressions.

Valid logical Operators are:

* __&&__ - if both the left and right subexpressions are true, resolves to true, otherwise false
* __||__ - if one or both of the left or right subexpressions are true, resolves to true, otherwise false
* __!__ - boolean "not"

### Other operators

* __(__ Open parenthesis
* __)__ Close parenthesis
* __+__ Addition symbol (for numbers and strings); also functions as a unary plus
* __-__ Subtraction symbol (for numbers and strings); also functions as a unary minus
* __*__ Multiplication symbol
* __/__ Division symbol
* __%__ Modulus (see the note bellow)
> * Given two numbers, the dividend a and the divisor n, a modulo n (abbreviated as "a % n") is the remainder. For instance, the expression "7 % 3" evaluates to 1, and "9 % 3" evaluates to 0.