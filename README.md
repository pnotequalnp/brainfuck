# Brainfuck
[Brainfuck](https://esolangs.org/wiki/Brainfuck) is an esoteric programming language. Its source
code consists only of 8 characters. Here is an example of a simple brainfuck program which prints
hello world to stdout:
```brainfuck
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```

## Usage
Basic usage compiles the source to LLVM and runs it with the LLVM JIT compiler.
```
$ brainfuck hello.bf
Hello World!
```

To generate an object file as output, use the `-c` flag. It will need to be linked against `libc`.
```
$ brainfuck -c hello.bf
$ ls
hello.bf  hello.o
```

To interpret the source directly without LLVM, use the `-i` flag. This is much slower than the other
options.
```
$ brainfuck -i hello.bf
Hello World!
```

See the help command for additional flags and usage information.
```
$ brainfuck --help
```

## Known Issues
- Only ASCII source code is supported
