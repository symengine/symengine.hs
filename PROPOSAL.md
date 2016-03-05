# Title

Create Haskell bindings for `SymEngine` by extending and wrapping the C FFI layer of `SymEngine`.

# About Me
## Personal Information

__Name:__ Siddharth Bhat

__Email:__ `siddu.druid@gmail.com`

__University:__ Manipal Institute of Technology, 4th semester, Computer Science Undergraduate

__IRC nick:__ `bollu`, on `#math`, `#haskell`, `#diagrams`, `#rust` at freenode

__Github username:__ `bollu`

__Other contact methods:__ Skype - `druidofdclaw`

__Country of residence:__ India

__Timezone:__ IST (GMT +5:30)

__Primary language:__ English

## Short Bio

I'm 19 years old, going to turn 20 on 10th July.

I enjoy math and programming immensely, math more than programming a lot of the time. I like the elegance of pure math, and I try to find programming languages that reflect that. I'd say Haskell, Rust and Python achieve that state of zen in wildly different ways. 

I play the piano as a hobby, and I've been trying to pick up the guitar. Cooking is something else that I love to do as well, though I'm not great at it.

## Me as a Programmer

### Operating Systems

I'm currently using Mac OS X, but I used to run ArchLinux. I still own Kali Linux and Windows 10, though they're not my daily drivers.

### Text Editors

I switch between a bunch of them depending on what I'm doing:

* If it's Python, then I generally use PyCharm / Sublime Text

* If it's C++, then Vim with YouCompleteMe.

* HTML/CSS/Javascript is Sublime Text and WebStorm

* General text editing is again mostly Vim

### Tooling

I contribute to a decent number of projects on Github, so `git` is something I use everyday.

As for shells, I use `zsh` with `oh-my-zsh`.

I prefer `ag`([silver searcher](https://github.com/ggreer/the_silver_searcher)) over `grep` due to
performance.

For one-off shell scripts, I tend to write Python scripts, or use Haskell's [turtle](https://hackage.haskell.org/package/turtle) library for "shell" scripting.

### Stuff I've contributed to

##### PSSSPP - A PSP Emulator

[Link to commits](https://github.com/hrydgard/ppsspp/commits?author=bollu)

I wrote a decent amount of code for PPSSPP, mainly related to the touch screen controls and


##### VisPy

[Link to commits](https://github.com/vispy/vispy/commits?author=bollu)

I programmed for VisPy during GSoC 2015 (and I still continue to work on it). My project was to rewrite
a large chunk of code with one of the mentors to improve performance and provide flexibility to future versions
of VisPy, and to integrate Cassowary into VisPy for nice-looking plots.


##### Haskell ecosystem

I have random commits into different Haskell projects. None of them are large enough to write about,
but they're small housekeeping stuff I took up while learning the language.

I also hang out on the Haskell IRC as `bollu` to answer questions and learn stuff.

##### Rust ecosystem

I have contributed to the Rust ecosystem here-and-there, reporting compiler bugs and sending PR's to
a couple of Rust packages.

I was also a member of Piston Developers, a group of Rust programmers who are trying to experiment with different
game engine architectures in Rust.

### Python and me.

I like the terse and yet expressive nature of python, and I dislike it for the exact same reason.

Python has brilliantly written libraries, and feels very natural to use in a `REPL` for
things like data exploration and web programming (and attacking).

One of the coolest things I've done in Python was to probably SQL inject databases during
Microsoft's Build the Shield competition to access a 20-character long password.

Python's ability for higher-order-functions and the awesome `requests` library saved the day.

[Code for SQL injection](https://gist.github.com/bollu/b77cefbb9094f1b782a7#file-bts-sql-injection-py-L66)


#### Most advanced feature I've used

I've probably used most Python features here-and-there: 

`decorators`, `generators`, even creating objects
on-the-fly, and other weird things. I prefer to stick to simple code though, since it aids comprehension
for everyone involved.

Some of the most advanced standard library features might be stuff like `multiprocessing` to parallelise an
attack on a particular server, and `pickle` to serialise data.


#### Cool python trick I like:

```python
# transpose a 2-d list with a zip

x = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
x_transpose = list(zip(*x))
```

#### Stuff I dislike about python

Python sometimes has _"too much magic"_, and I've submitted pull requests to actually reduce the amount
of action-at-a-distance that Python allows.

__Example PR to reduce magic__: [don't walk the module tree to create custom objects at import time](https://github.com/vispy/vispy/pull/1024).

The lack of types is a huge pain point as well, and is the reason I've been gravitating towards
languages with strong type systems like Rust and Haskell in the recent past.

### Favourite `Sympy` feature

[TODO: Need to fill this up]

## Stuff I've created

I've written a sublime text plugin called [SublimeBookmarks](https://packagecontrol.io/packages/Sublime%20Bookmarks) to
manage bookmarks in Sublime Text.

There's another tool called [teleport](https://hackage.haskell.org/package/teleport), to manage directories of projects.

Unfortunately, this one is much rougher around the edges both due to Haskell's own limitations in distribution,
and due to my schedule that didn't let me polish it. I do plan on coming back to this and showing it some love
over the summer.


I've started many toy language projects, though none of them are complete. In no real order:

* [A Lisp interpreter in C](https://github.com/bollu/lispInterpreter)

* [Achilles - Rust implementation](https://github.com/bollu/rust-achilles) a toy programming language of my own
    called "achilles". It was going to be functional, with heavy inspiration from Haskell. Rust's closure + lifetime
    interaction problems during beta made me give this one up.

* [cellular Automata](https://github.com/bollu/cellularAutomata) - A collection of cellular automata written in Haskell. This is written so it's more of a library than an executable, though it does contain sample cellular automata. The goal was to make pretty GIF's with a neat API (using commands).


# Me and my project


The goal of the project is to create well formed Haskell bindings from `SymPyEngine` to Haskell. As of now,
`SymPyEngine` is a subset of `SymPy` (in terms of feature set), and does __not__ have `SymPy` running on it yet, though that is in the roadmap.

[Github issue for migration](https://github.com/symengine/symengine/wiki/SymPy-core-upgrade-to-SymEngine).

`SymPyEngine`'s existence is crucial because it allows for a C++ compiled codebase for symbolic computation, which is much faster than Python, and is also much more portable (most languages have C/C++ FFI bindings).  

#### Why this project?

I'd love to do this for two reasons:

1) I like math, and I'm sure that by spelunking the SymPyEngine codebase, I'll pick up some amount
   of math / algorithms / optimization lore by osmosis. So that makes the project "fun" as a whole

2) I'd get to push Haskell's story of symbolic mathematics, something that is somewhat lacking
   right now [as agreed upon by the Haskell community](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#numerical-programming).
   `SymPy` bindings would be *huge* since the toolkit is mature, and would encourage a lot
   of interesting code in Haskell.

I have some selfish interest in this as well - I wanted to create really slick visualisations for
my blog in relation to Group Theory (since I find the subject beautiful). Haskell has a brilliant
library for visualisations called [diagrams](http://projects.haskell.org/diagrams/).

However, it lacks a proper library for symbolic math. Getting this into Haskell would mean
cooler libraries to use and better data visualisation for me :)

#### Qualifications

I've written a decent amount of Haskell, C, and Python code in my past. This project needs all 3 of
those languages - C for the FFI, Haskell for the Haskell bindings, Python to look at the Cython
wrappers and read `SymPy` code in general.

I also understand pure math (I've taken analysis, algebra and topology, though I know more algebra
than the other two branches of math), so I won't get "lost" in the docs so to speak. 


## Timeline

I'll be dedicating 8 hours a day, hopefully every day. It'll probably be a little less during weekends,
but not a drastic drop in productivity. That brings it to ~50 hours a week, give or take.

As far as I am aware, This has _not_ been done in the past, so there is no real "reference"
implementation to go to.

####[TODO: I need help breaking this down]


```
23 May  Students begin coding for their Google Summer of Code projects; Google begins issuing initial student payments provided tax forms are on file and students are in good standing with their communities.
Work Period Mentors give students a helping hand and guidance on their projects.
20 June 19:00 UTC   Mentors and students can begin submitting mid-term evaluations.
27 June 19:00 UTC   Mid-term evaluations deadline; Google begins issuing mid-term student payments provided passing student survey is on file.
Work Period Mentors give students a helping hand and guidance on their projects.
15 August - 23 August 19:00 UTC Final week: Students tidy code, write tests, improve documentation and submit their code sample. Students also submit their final mentor evaluation.
```

#### Week 1, 2 - [23 May to 6 June]
#### Week 3 - [6 June to 13 June]
#### Week 4, 5 - [13 June to 27 June] Mid term Evaluation prep
### 27 June - Mid term Evaluation

#### Week 6, 7 - [27 June to 11 July]
#### Week 8, 9 - [11 July to 25 July]
#### Week 10, 11 - [25 July - 8 August]
#### Week 12, 13 - [8 August - 22 August] Final Evaluation prep

