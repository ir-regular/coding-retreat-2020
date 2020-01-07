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

## Language Learning

### Erlang resources

- :link: [Getting Started with Erlang: User's Guide](http://erlang.org/doc/getting_started/users_guide.html)
- :link: [OTP Design Principles User's Guide](http://erlang.org/doc/design_principles/users_guide.html)
- :link: [Mnesia User's Guide](http://erlang.org/doc/apps/mnesia/users_guide.html)
- :book: :link: [Learn You Some Erlang](https://learnyousomeerlang.com/content)
- :book: [Programming Erlang. Software for a Concurrent World](http://shop.oreilly.com/product/9781937785536.do) by Joe Armstrong
- :book: [Erlang Programming. A Concurrent Approach to Software Development](http://shop.oreilly.com/product/9780596518189.do) 
- :book: [Designing for Scalability with Erlang/OTP](http://shop.oreilly.com/product/0636920024149.do) by Francesco Cesarini, Steve Vinoski
- :book: :link: [Études for Erlang](https://github.com/oreillymedia/etudes-for-erlang)
- :book: :link: [The Erlang Runtime System](https://blog.stenmans.org/theBeamBook/)
- :notebook: :link: [Making reliable distributed systems in the presence of software errors](https://erlang.org/download/armstrong_thesis_2003.pdf), Joe Armstrong's 2003 PhD thesis
- :woman_student: [An Erlang Course](https://www.erlang.org/course) from the official website
- :woman_student: University of Kent [Erlang master classes](https://www.cs.kent.ac.uk/ErlangMasterClasses/)

### C resources

- :book: [Modern C](https://modernc.gforge.inria.fr/), Jens Gustedt
- :book: [21st Century C](http://shop.oreilly.com/product/0636920025108.do), Ben Klemens
- :link: [Linux kernel coding style](https://www.kernel.org/doc/html/latest/process/coding-style.html)
- :link: [How to C in 2016](https://matt.sh/howto-c)

### C++ resources

- :link: [The Definitive C++ Book Guide and List](https://stackoverflow.com/questions/388242/the-definitive-c-book-guide-and-list)
- :link: [Learning C++](https://blog.tartanllama.xyz/learning-cpp/), Sy Brand - article, includes links to good blogs
- :card_index_dividers: [C++ 2003](https://ankiweb.net/shared/info/1219517843) - a public Anki deck based on C++ Primer, 4th edition

## Write a toy compiler

I have actually written more than one interpreter over the years, but never had to deal with a "...to assembly" part, or optimization.

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

## Write a toy filesystem

As it happens "a toy filesystem" is something I've done previously (access to an e-mail inbox via POP, iirc). It was fun and I'd like to apply the same principle to something else. Really, I just want to play around with systems programming. And so:

- [You can be a kernel hacker!](https://jvns.ca/blog/2014/09/18/you-can-be-a-kernel-hacker/)
- [Eudyptula challenge](https://github.com/agelastic/eudyptula) (someone else's repo, since the original challenge is defunct)
- https://wiki.osdev.org/Main_Page
- https://kernelnewbies.org/
- :book: "Modern Operating Systems", Andrew S. Tanenbaum
- :book: ["Operating Systems: Three Easy Pieces"](http://pages.cs.wisc.edu/~remzi/OSTEP/), Remzi H. Arpaci-Dusseau and Andrea C. Arpaci-Dusseau
- :book: ["Advanced Linux Programming"](https://www.oreilly.com/library/view/advanced-linux-programming/0735710430/), Alex Samuel, Jeffrey Oldham, Mark Mitchell

## Try at least one CTF

Turns out I messed up terminology: CTFs are competitions with specific dates, whereas online challenges are called wargames. Fine. I want to try solving at least one wargame.

### Learning materials

- :link: [How to Get Started in CTF](https://www.endgame.com/blog/technical-blog/how-get-started-ctf) with a list of books etc
- :link: [What is CTF and how to get started!](https://dev.to/atan/what-is-ctf-and-how-to-get-started-3f04) learning resources, links to challenges etc
- :book: :link: [CTF Field Guide](https://trailofbits.github.io/ctf/)
- :book: :link: [CTF Resources](https://ctfs.github.io/resources/)

### Wargames

- [OverTheWire](https://overthewire.org/wargames/) teaches some of the basic skills
- [cryptopals](https://cryptopals.com/), cryptography challenges
- [Micro Corruption](https://microcorruption.com/login) teaches assembly, low-level debugger use
- [Smash the Stack](http://smashthestack.org/wargames.html)
- [a list of permanent CTF challenges / wargames](http://captf.com/practice-ctf/)

## Put together a cyberdeck

:sob::point_right::pound:

- [/r/cyberDeck](https://www.reddit.com/r/cyberDeck/)
- [/r/MechanicalKeyboards](https://www.reddit.com/r/MechanicalKeyboards/)
- [/r/MechanicalKeyboardsUK](https://www.reddit.com/r/MechanicalKeyboardsUK/)
- [/r/AnnePro](https://www.reddit.com/r/AnnePro/)
- [NXP i.MX7D Starter Kit](https://shop.technexion.com/pico-pi-imx7-startkit-rainbow-hat.html) for Android Things
- there exists a [London Hackspace](https://london.hackspace.org.uk/) except, annoyingly, the whole of London is in the way

## General resources

- https://learnawesome.org/
- [Emoji that work in Github](https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md), a cheatsheet
