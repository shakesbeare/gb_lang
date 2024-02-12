# About Gb

Gb is a project I created to learn more about the inner workings of programming
languages. While it is not fully functional, it has achieved this goal and (usually)
works within its extremely constrained scope. 

# Does it work?

Probably not. This is a personal project and I'm largely using Git as a means of cloud
storage in addition to version control. So, very frequently, the main branches will be
broken. 

# Why not use a parser generator?

Since Gb was primarily a learning exercise, I wanted to try and do as much as possible
of it from scratch. Armed with only my programming languages textbook and some serious
willpower, I created Gb by myself as an extracurricular project. There are a few
branches where I have experimented with using parser generators (like Pest), but
ultimately, these efforts weren't as fun for me as it was to build it from the ground
up.

# The Process

Building anything resembling a programming language is a hard task. The gap between the
theory and exercises presented in classes and building an entire system from scratch is
pretty large! With that in mind, there are a handful of compromises I made in building
the current version of Gb to enable myself to learn the most possible and still come out
the other end with something that might resemble a functional program. 

First of all, I chose to use Rust. Not only is Rust extremely fun to use, but it
presents unique advantages for this sort of exploration project that other languages in
it's class don't have. Namely, its memory safety. While I may have to fight with Rust
itself a lot more, I feel that this choice generally allows me to focus on the details
of the project I'm building more than debugging inane issues that might distract from
the project at large. 

Building off of that, I chose not to worry about writing perfectly idiomatic or even
very well optimized code since, again, that wasn't the objective of the project. I
wanted to leverage Rust's compiler to help guard me against mistakes without feeling
particularly slowed down by it or the general style-paradigms standard in the language. 

A word on style: this project contains a few very large functions, especially noticable
in the parser code. This is by design. I felt that making a monolithic parsing function,
while more difficult to manage while coding, made the "state machine" foundations of the
code more clear and helped me reason about it theoretically. As (~~if~~) the project grows, it
will certainly be refactored into something more easily readable and maintainable. But,
as it stands, I like the theoretical benefits the linearly visible state machine
provides.

# The future of Gb

I occasionally resume work on Gb. My plan is to one day make it a sleek scripting
language with powerful support interop with Rust. As of now, development of the project
has taken the back seat to my studies in college. 
