# About Gb

Gb is a project I created to learn more about the inner workings of programming
languages. While it is not fully functional, it has achieved this goal and (usually)
works within its extremely constrained scope. 

# Does it work?

The main branch *should* always be at least minimally functional at all times, but 
I won't guarantee, at this time, that the project will be useful. Specifically,
the Gb interpreter is currently not built at all. There are versions in the history
with a working interpreter, but the main branch doesn't have one, at the moment.

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
wanted to switch to a fancier Pratt-parser rather than my grammar-based parser of this version. 


# The future of Gb

I occasionally resume work on Gb. My plan is to one day make it a sleek scripting
language with support for interop with Rust. As of now, development of the project
has taken the back seat to my studies in college. 
