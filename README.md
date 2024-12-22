![CI](https://github.com/shakesbeare/gb_lang/actions/workflows/rust.yml/badge.svg)
# About Gb

Gb is a project I created to learn more about the inner workings of programming
languages. While it is not fully functional, it has achieved this goal and (usually)
works within its extremely constrained scope. 

# Why Gb? 

Gb is intended to be a lightweight scripting language that combines the type system of
Rust with the ease of use of Python. Combine a familiar syntax with an automatic command
line argument parsing (not implemented), Gb should be perfect for small CLI automations
in your daily workflow. 

# Does it work?

Yes! The body of the interpreter is finished and the language can be used for simple
calculations. The standard library is extremely sparse and error handling and messages
can often be improved. 

The REPL is very rudimentary, at the moment.

## Using Cargo

```
cargo r                    # for REPL
cargo r -- my_cool_file.gb # to run a file
```

## Standalone

```
cargo b -r                          # build the project
./target/release/gb                 # for REPL
./target/release/gb my_cool_file.gb # to run a file
```

# Features

So, What makes Gb unique? Here are a few present and planned features on the docket for
Gb. 

- Top level code with auto-executing main function (if present)
- Strong, inferred typing (not implemented)
- Automatic parsing of command line arguments (not implemented)
- First class support for interpreted, JIT, and compiled modes (1/3 complete)
- Algebraic Data Types (not implemented)

# Code Sample

Gb is a very familiar language in its very early stages. While it looks largely
unassuming now, it should gain additional character as development progresses.

```
fn main() {
    let x = 0;
    while x < 10 {
        x = x + 1;
    }
    std.print(x);

    let foo = fn() { std.print("Hello, foo!"); };
    
    x = 0;
    while x < 10 {
        foo();
        x = x + 1;
    }
}
```
```
Outputs:
> 10
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
> Hello, foo!
```


## Recursion!
```
fn main() {
    std.print(fib(10));
}

fn fib(n) {
    if n == 0 {
        return 0;
    } else if n == 1 {
        return 1;
    }

    return fib(n - 1) + fib(n - 2);
}
```
```
Outputs:
> 55
```

## Optional Main Function

Gb will automatically execute any top-level function named `main`, but this is
not required. The following is just as valid as the previous examples.

```
std.print("Hello, World!");
```
```
Outputs:
> Hello, World!
```

## Working with multiple files

Gb automatically attempts to load a file when namespace lookup fails. Folders are
currently not supported. In the future, you will be able to alias paths to types.
```
// foo.gb
fn foo() {
    std.print("Hello from foo!");
}
```
```
// file.gb
fn main() {
    foo.foo();
}
```
```
Outputs:
> Hello from foo!
```

# Why not use a parser generator?

Since Gb was primarily a learning exercise, I wanted to try and do as much as possible
of it from scratch. Armed with only my programming languages textbook and some serious
willpower, I created Gb by myself as an extracurricular project. There are a few
branches where I have experimented with using parser generators (like Pest), but
ultimately, these efforts weren't as fun for me as it was to build it from the ground
up.

# The Process

Gb has been built and rebuilt a few different times. The current iteration is based on
Thorsten Ball's book [Writing an Interpreter in Go](https://thorstenball.com/books/),
which ended up being quite a convenient resource because the syntax for Monkey was nearly
identical to what I had planned for Gb. But the initial versions were built entirely on
my own. 

In specific, [this](https://github.com/shakesbeare/gb_lang/tree/FromTheGroundUp)
branch represents the work I was able to put in with a parser generator. As mentioned above,
I ended up being pretty unhappy with this approach. I especially disliked how difficult it
was to hook into the Pest error system to emit useful error messages from Gb. 

It's also worth checking out [this](https://github.com/shakesbeare/gb_lang/tree/Rewrite) branch,
which was the version I had before I began to use the *Writing an Interpreter in Go* version. 
I learned the most from this version and ultimately decided to rewrite it again only because I
wanted to switch to a fancier Pratt-parser rather than the grammar-based parser of this version. 


# The future of Gb

I occasionally resume work on Gb. My plan is to one day make it a sleek scripting
language with support for interop with Rust. As of now, development of the project
has taken the back seat to my studies in college. 

