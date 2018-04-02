# Using CommsRouter Expressions
</br>
## Introduction

CommsRouter uses two flavors of expression syntax for binding Agents to Queues and filtering Tasks into different Queues in a Plan configuration. Those are RSQL and JEVAL. Expressions are composed of constants, functions, as well as logical and comparison operators.

For example, a Queue expression can look like:

```
RSQL:  "skills==electronics and language=in=('en','ru','es')"
or
JEVAL: "HAS(#{skills},'electronics') && IN(#{language},['en','ru','es'])"
```

And a Plan filter expression can look like:

```
RSQL:  "customer_value=='Gold' and type=='ticket'"
or
JEVAL: "#{customer_value} == 'Gold' && #{type} == 'ticket'"
```
</br>

## RSQL Expression Structure
_The following grammar specification is written in EBNF notation (ISO 14977)._

### Examples

Examples of RSQL expressions in both FIQL-like and alternative notation:
```
- name=="Kill Bill";year=gt=2003
- name=="Kill Bill" and year>2003
- genres=in=(sci-fi,action);(director=='Christopher Nolan',actor==*Bale);year=ge=2000
- genres=in=(sci-fi,action) and (director=='Christopher Nolan' or actor==*Bale) and year>=2000
- director.lastName==Nolan;year=ge=2000;year=lt=2010
- director.lastName==Nolan and year>=2000 and year<2010
- genres=in=(sci-fi,action);genres=out=(romance,animated,horror),director==Que*Tarantino
- genres=in=(sci-fi,action) and genres=out=(romance,animated,horror) or director==Que*Tarantino
```


### Logical Operators

RSQL expression is composed of one or more comparisons, related to each other with logical operators:

  -  `;`  or  `and`  &nbsp; - &nbsp; Logical AND

  -  `,` or `or`  &nbsp; - &nbsp; Logical AND

By default, the AND operator takes precedence (i.e. it’s evaluated before any OR operators are). However, a parenthesized expression can be used to change the precedence, yielding whatever the contained expression yields.

```
input          = or, EOF;
or             = and, { "," , and };
and            = constraint, { ";" , constraint };
constraint     = ( group | comparison );
group          = "(", or, ")";
```


### Comparison Operators

Comparison is composed of a selector, an operator and an argument.

```
comparison     = selector, comparison-op, arguments;
```

<br />
Selector identifies a field (or attribute, element, …) of the resource representation to filter by. It can be any non empty Unicode string that doesn’t contain reserved characters (see below) or a white space. The specific syntax of the selector is not enforced by this parser.

```
selector       = unreserved-str;
```


Comparison operators are in FIQL notation and some of them has an alternative syntax as well:

  -  `==`  *equals* or *has* &nbsp;&nbsp;&nbsp;&nbsp;  
    - Example with name=*John*,  names=*(Ivan, John, Kate)*</br>
        `name=='John'   -> true`<br>
        `names=='John'  -> true`<br>
        `name=='Smith'  -> false`<br>
        `names=='Smith' -> false`<br>



  -  `!=`  the opposite of `==`
      - Example with name=*John*,  names=*(Ivan, John, Kate)*</br>
          `name=='John'   -> false`<br>
          `names=='John'  -> false`<br>
          `name=='Smith'  -> true`<br>
          `names=='Smith' -> true`<br>



  -  `=lt=` or `<` less then


  -  `=le=` or `<=` less than or equal to


  -  `=gt=` or `>` greater than operator


  -  `=ge=`or `>=` greater than or equal to


  -  `=in=` contains
    - Example with name=*John*<br>
      `name=in=(Oliver,Harry,Louis)  -> false`<br>
      `name=in=(Oliver,John,Louis)   -> true`<br>



  -  `=out=` the opposite of `=in=`
    - Example with name=*John*<br>
        `name=out=(Oliver,Harry,Louis)  -> true`<br>
        `name=out=(Oliver,John,Louis)   -> false`<br>



### Arguments
Argument can be a single value, or multiple values in parenthesis separated by comma. Value that doesn’t contain any reserved character or a white space can be unquoted, other arguments must be enclosed in single or double quotes.

```
arguments      = ( "(", value, { "," , value }, ")" ) | value;
value          = unreserved-str | double-quoted | single-quoted;

unreserved-str = unreserved, { unreserved }
single-quoted  = "'", { ( escaped | all-chars - ( "'" | "\" ) ) }, "'";
double-quoted  = '"', { ( escaped | all-chars - ( '"' | "\" ) ) }, '"';

reserved       = '"' | "'" | "(" | ")" | ";" | "," | "=" | "!" | "~" | "<" | ">";
unreserved     = all-chars - reserved - " ";
escaped        = "\", all-chars;
all-chars      = ? all unicode characters ?;
```
If you need to use both single and double quotes inside a quoted argument, then you must escape one of them using __\__ (backslash). If you want to use __\__ literally, then double it as __\ \__. Backslash has a special meaning only inside a quoted argument, not in unquoted argument.

</br>


## JEVAL Expression Structure

Expressions are made up of simple comparisons of constants and/or JSON keys, grouped together by parenthesis functions, and logical operators.

### Examples

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

* ```\>```  greater than

* ```\>=``` greater than or equal to

* ```=``` equals

* ```!=``` does not equal

* ```<=``` less than or equal to

* ```<```  less than

* `HAS(#{array}, 'value')` - does array in the first argument contain the value in the second argument

* `IN('value', #{array})` - does the scalar value on the first argument exists in the array on the second argument.

* `CONTAINS(#{value1}, #{value2})` - does the scalar value on the left contain the scalar value on the right.

### Logical Operators

Compares the results of sub-expressions to the left and right and return true or false based on the operand. Parenthesis can be used to group sub-expressions.

Valid logical Operators are:

* `&&` &nbsp; - if both the left and right subexpressions are true, resolves to true, otherwise false
* `||` &nbsp; - if one or both of the left or right subexpressions are true, resolves to true, otherwise false
* `!`  &nbsp;&nbsp;&nbsp; - boolean "not"

### Other operators

- `(` Open parenthesis
- `)` Close parenthesis
- `+` Addition symbol (for numbers and strings); also functions as a unary plus
- `-` Subtraction symbol (for numbers and strings); also functions as a unary minus
- `*` Multiplication symbol
- `/` Division symbol
- `%` Modulus (see the note bellow)
  - Given two numbers, the dividend a and the divisor n, a modulo n (abbreviated as "a % n") is the remainder. For instance, the expression "7 % 3" evaluates to 1, and "9 % 3" evaluates to 0.
