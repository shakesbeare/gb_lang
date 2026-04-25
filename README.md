![CI](https://github.com/shakesbeare/gb_lang/actions/workflows/rust.yml/badge.svg)
# About Gb

Gb is a project I created to learn more about the inner workings of programming
languages. While it is not fully functional, it has achieved this goal and (usually)
works within its extremely constrained scope. 

# Does it work?

No! The main branch is currently my development branch because I am lazy. Find the last working version since I tore everything apart [here](https://github.com/shakesbeare/gb_lang/releases/tag/LastWorking).

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

In specific, [this](https://github.com/shakesbeare/gb_lang/releases/tag/FromTheGroundUp)
tag represents the work I was able to put in with a parser generator. As mentioned above,
I ended up being pretty unhappy with this approach. I especially disliked how difficult it
was to hook into the Pest error system to emit useful error messages from Gb. 

It's also worth checking out [this](https://github.com/shakesbeare/gb_lang/releases/tag/Rewrite) tag,
which was the version I had before I began to use the *Writing an Interpreter in Go* version. 
I learned the most from this version and ultimately decided to rewrite it again only because I
wanted to switch to a fancier Pratt-parser rather than the grammar-based parser of this version.

I'm currently in the middle of restarting the whole thing from scratch using the knowledge I've gained
along the way. In particular, the following things were pain points for me:

1) The AST was extremely cumbersome to navigate in the interpreter
2) Adding new kinds of nodes to the AST added a lot of boilerplate which made it less enjoyable to work on
3) The lexer was pretty clunky as well, also requiring a handful of lines to add new tokens to
4) I really wanted the lexer to be an iterator and to handle the entire tokenization process with minimal allocations (mostly as an exercise, I'm not expecting the performance to be anything incredible but making problems harder is fun)
5) The interpreter was honestly a disaster that felt full of hacks at every turn and I wanted to give myself a better foundation to start with so I could have a better opportunity to make good decisions

