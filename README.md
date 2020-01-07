## General Wishlist

At some point, I would like to write:

- a toy compiler
- a toy debugger
- a toy database, relational
- a toy filesystem
- my pet idea for a GraphQL resolution engine
- try at least one CTF
- put together a cyberdeck 

and also I need to learn Erlang.

## Learn Erlang

### Docs

- [Getting Started with Erlang: User's Guide](http://erlang.org/doc/getting_started/users_guide.html)
- [OTP Design Principles User's Guide](http://erlang.org/doc/design_principles/users_guide.html)
- [Mnesia User's Guide](http://erlang.org/doc/apps/mnesia/users_guide.html)

### Books

- [Learn You Some Erlang](https://learnyousomeerlang.com/content)
- [Programming Erlang. Software for a Concurrent World](http://shop.oreilly.com/product/9781937785536.do) by Joe Armstrong
- [Erlang Programming. A Concurrent Approach to Software Development](http://shop.oreilly.com/product/9780596518189.do) 
- [Designing for Scalability with Erlang/OTP](http://shop.oreilly.com/product/0636920024149.do) by Francesco Cesarini, Steve Vinoski
- [Études for Erlang](https://github.com/oreillymedia/etudes-for-erlang)
- [The Erlang Runtime System](https://blog.stenmans.org/theBeamBook/)

### Papers

- [Making reliable distributed systems in the presence of software errors](https://erlang.org/download/armstrong_thesis_2003.pdf), Joe Armstrong's 2003 PhD thesis

### Courses

- [An Erlang Course](https://www.erlang.org/course) from the official website
- University of Kent [Erlang master classes](https://www.cs.kent.ac.uk/ErlangMasterClasses/)

## Write a toy compiler

I have actually written more than one interpreter over the years, but never had to deal with a "...to assembly" part, or optimization. Also, I'd like to remind myself of how C works, so this section includes some C resources.

### Online

- [Let's Build a Compiler](https://compilers.iecc.com/crenshaw/), Jack Crenshaw
- [Writing a C Compiler](https://norasandler.com/archive/), Nora Sadler (based on Ghuloum’s paper)
- [Someone's nice summary on Stackoverflow](https://softwareengineering.stackexchange.com/a/165558)

### Books

- [Crafting Interpreters](https://craftinginterpreters.com/), Bob Nystrom
- Compilers: Principles, Techniques, and Tools, Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman
- [Modern Compiler Design](https://dickgrune.com/Books/MCD_2nd_Edition/), Dick Grune, Henri E. Bal, Ceriel J.H. Jacobs, Koen G. Langendoen
- Compiler Construction, Niklaus Wirth

### Papers

- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf), Abdulaziz Ghuloum
- [A Nanopass Framework for Compiler Education](https://www.semanticscholar.org/paper/A-Nanopass-Framework-for-Compiler-Education%E2%88%97-Sarkar-Waddell/ef63ccac82166f1062b05c79c00e8930c191307a), Dipanwita Sarkar, Oscar Waddell, R. Kent Dybvig

### Tools

- [LLVM](https://llvm.org)

### C resources

- [Modern C](https://modernc.gforge.inria.fr/), Jens Gustedt
- [21st Century C](http://shop.oreilly.com/product/0636920025108.do), Ben Klemens
- [Linux kernel coding style](https://www.kernel.org/doc/html/latest/process/coding-style.html)
- [How to C in 2016](https://matt.sh/howto-c)

### C++ resources

- [The Definitive C++ Book Guide and List](https://stackoverflow.com/questions/388242/the-definitive-c-book-guide-and-list)
- [Learning C++](https://blog.tartanllama.xyz/learning-cpp/), Sy Brand (includes links to good blogs)

## Write a toy debugger

And now I'm glad I included those C books up there!

- [Writing a Linux Debugger](https://blog.tartanllama.xyz/posts/), Sy Brand
- [How debuggers work](https://eli.thegreenplace.net/tag/debuggers), Eli Bendersky (and also follow the linked references)
- [How debuggers really work](https://github.com/levex/debugger-talk/blob/master/article_opensourcecom/article.md), Levente Kurusa, and the talk: [slides](https://github.com/levex/debugger-talk/tree/master/talk) and [recording](https://archive.org/details/lca2018-Lets_write_a_Debugger)
- [Loading and ptrace'ing a process on Linux](http://system.joekain.com/2015/06/08/debugger.html), part of a series [Writing a Debugger](http://system.joekain.com/debugger/)

## Write a toy relational database

- [Let's build a simple database](https://cstack.github.io/db_tutorial/), someone did this already :)
- [Readings in Database Systems](http://www.redbook.io) aka "Red Book"
- [CMU Advanced Database Systems](https://15445.courses.cs.cmu.edu/fall2019/) course (incl. projects, course videos)
- [Architecture of a Database System](https://dsf.berkeley.edu/papers/fntdb07-architecture.pdf), Joseph M. Hellerstein, Michael Stonebraker and James Hamilton
