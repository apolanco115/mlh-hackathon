# mlh-hackathon

### Prerequisites

To begin working with this project you must have the [Haskell Platform](https://www.haskell.org/platform/) installed

### Install and Run

Once cloned `cd` into the `pong` directory and run the following commands:

```
runhaskell Setup.hs configure —user
runhaskell Setup.hs build
runhaskell Setup.hs install
cabal run
```

## Inspiration

When deciding what project to do and which languages to use, we wanted something totally new and challenging for us. As such, we settled on creating a two player pong game using Haskell, a functional programming language completely unfamiliar to us and different than what we were used to.

## What it does

This game is a simple pong game, like ping pong, in which you control paddles to hit a ball and change its direction and velocity. This game was intended to allow two players to play the same game over the internet.

## How I built it

We built this game in pure Haskell using libraries such as gloss for graphics and Network.Socket to allow it to be played over the internet. The idea was so that one user can connect to another user's instance (the host) via web sockets.

## Challenges We ran into

We ran into a number of issues including the learning curve for working with Haskell, setting up the development environment for the first time (Haskell can be a bit tricky to set up), installing the proper dependencies needed, but namely, implementing the web sockets. We managed to build a working pong game with graphics and user input, and we also managed to build a working prototype of web sockets sending text back and forth, similar to a chat client, however we ran into lots of issues getting the web sockets to work with our game's front end. We spent over 16 hours working on the implementation of the web sockets and were still unable to fully develop it due to our inexperience using the Haskell language and the lack of documentation and resources available over the internet.

## Accomplishments that We're proud of

We are very proud to have learned the extensive and quirky features of Haskell that make it different from other languages like Java, C#, and C++. We are also proud to have been able to program a working user interface for the pong game that responds to user input and behaves as a pong game should.

## What We learned

We learned that Haskell is a very niche but powerful language closely resembling logic used in math. We learned how to create our own data types and data structures and implement our own functions. We also learned about ways to package, distribute, and compile Haskell programs.

## What's next for Haskell Pong

We fully intend on finishing our implementation of the web sockets and feature our program on GitHub and in the Haskell package manager, Hackage, so follow us on GitHub and stay tuned!
