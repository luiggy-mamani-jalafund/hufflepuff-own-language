# Interpreter
![](./mountain.png)

## Frontend:
1. Step Lexer
2. Step Parser
3. Step Syntax analyser

## Backend:
### Tree Walk Interpreter
Some programming languages begin executing code right after parsing it to an AST
(with maybe a bit of static analysis applied). To run the program, the interpreter
traverses the syntax tree one branch and leaf at a time, evaluating each node as it
goes.
Some people use “interpreter” to mean only these kinds of implementations, but
others define that word more generally, so I’ll use the inarguably explicit tree-walk
interpreter

### Transpilers
You write a front end for your language. Then, in the back end, instead of doing all the
work to lower the semantics to some primitive target language, you produce a
string of valid source code for some other language that’s about as high level as
yours. Then, you use the existing compilation tools for that language as your escape
route off the mountain and down to something you can execute.
They used to call this a source-to-source compiler or a transcompiler. After the rise
of languages that compile to JavaScript in order to run in the browser, they’ve affected
the hipster sobriquet transpiler.While the first transcompiler translated one assembly language to another, today, most
transpilers work on higher-level languages. After the viral spread of UNIX to machines
various and sundry, there began a long tradition of compilers that produced C as their
output language. C compilers were available everywhere UNIX was and produced
efficient code, so targeting C was a good way to get your language running on a lot of
architectures.
Web browsers are the “machines” of today, and their “machine code” is JavaScript, so
these days it seems almost every language out there has a compiler that targets JS
since that’s the main way to get your code running in a browser.
The front end—scanner and parser—of a transpiler looks like other compilers. Then, if
the source language is only a simple syntactic skin over the target language, it may skip
analysis entirely and go straight to outputting the analogous syntax in the destination
language.
If the two languages are more semantically different, you’ll see more of the typical
phases of a full compiler including analysis and possibly even optimization. Then,
when it comes to code generation, instead of outputting some binary language like
machine code, you produce a string of grammatically correct source (well, destination)
code in the target language.
Either way, you then run that resulting code through the output language’s existing
compilation pipeline, and you’re good to go.
Compiling is an implementation technique that involves translating a source language to some other usually lower-level form. When you generate bytecode or machine code, you are
compiling. When you transpile to another high-level language, you are compiling too.
When we say a language implementation “is a compiler”, we mean it translates source code
to some other form but doesn’t execute it. The user has to take the resulting output and run it themselves.
Conversely, when we say an implementation “is an interpreter”, we mean it takes in source
code and executes it immediately. It runs programs “from source”.