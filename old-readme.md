# Infinite Runner Game

In this project, we will create a 2D infinite runner game in Haskell. Akin to Chrome's Dinosaur Game or Flappy Bird, this game will consist of simple
mechanics and a fun gameplay loop that can entertain for hours. Taking on this task, in a field dominated by object-oriented or procedural languages, will provide unique learning opportunities, and really push the boundaries of what functional languages can do, especially in terms of IO.

Leave the following sentence in so you can easily link back to the requirements and _especially_ rubric while editing your project:

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

## Team Members

Our team is:

- Matthew Smith (51209682)
- Mo Fardinzaman (57779167)
- Nicolas Mercuri (18981167)
- Danielle Lim (33485137)
- Devin Chikhlia (80666621)

## Acknowledgments

We surely built on the work of others! Here are resources and people we got support from:

- The quickstart guide in Haskell's [game-dev page](https://haskell-game.dev/packages/sdl2.html)
- General guide from https://github.com/JeremiahCheatham/Yellow-Snow/blob/main/Haskell-SDL2/app/Main.hs
- Haskell Wiki's instructions on installing SDL2 on Windows https://wiki.haskell.org/SDL/Windows
- Windows SDL2 installation via stack https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/
- GitHub issue for windows sdl2 installation via stack https://github.com/haskell-game/sdl2/issues/277

## Product Pitch (Nick, Danielle)

### Project Proposal: Infinite Runner Game in Haskell

#### Vision

The project aims to develop an infinite runner game similar to Chrome’s Dino game, where a player-controlled sprite navigates through a dynamic environment with diverse obstacles and an increasing level of difficulty over time.

The driving force behind this project is to expand our knowledge of Haskell by using it for a fun and unconventional purpose (real-time game development). This will challenge the boundaries of functional programming to be able to manage mutable state, handle real-time user input, update the game environment, and render the display efficiently via SDL2 (Simple DirectMedia Layer). The project will allow us to deepen our understanding of I/O Monads, while exploring new concepts including the integration of modules and design patterns related to game development that weren't covered in CPSC 312.

#### Minimum Viable Product: A simple 1-level version of the Infinite Runner game with:

- A single type of obstacle moving at a static pace
- A basic scoring system that increments over time
- Simple collision detection
- Game over detection when the player collides with an obstacle
- The ability to restart the game after losing

#### Proof of Concept

A working SDL2 window that displays a basic rectangular red sprite that the player can move left and right using keyboard inputs. This will serve as the foundation for the runner mechanics by showing the ability to handle basic input, rendering, and movement logic using Haskell and SDL2.

## Minimal Viable Project (Mo, Devin)

The MVP will focus on implementing the core mechanics of our endless runner game. This will focus on player movement, obstacle generation, and collision detection. The game will continuously generate obstacles that the player must avoid by jumping up and down whilst the game moves the screen for you. We will concentrate on building a basic game loop that handles user input, a basic scoring system that increments as time progresses, simple collision detection to trigger a game-over state, and the option to restart the game after a loss.

This MVP is a scaled-down version of the full project, allowing us to focus on the fundamental mechanics without the need for animations, character sprites, diverse level generations/obstacles, complex movements, or power-ups. By concentrating on the essential elements, we can ensure that the project is manageable and achievable within the shorter timeframe while still providing a soild gameplay experience. This allows to refine the core user experience before expanding on them in a final version.

This MVP builds on the strengths of Haskell's functional programming paradigms to manage game state and logic. Furthermore, Haskell's immutability ensures that state updates, such as player positions, obstacle locations, and game progression, are handled predictably. Recursion is ideal for iterating over the game loop, allowing us to cleanly manage repeated actions like updating the screen adn checking for collisions. Additionally, Haskell's strong type system allows us to define custom types for game entities, ensuring that data is handled consistently adn reducing the likelihood of runtime errors. Finally, the type class system allows us to easily expand on the game in the future.

Working on this project will naturally lead to learning and applying advanced Haskell concepts, such as monads, which is key for managing side effects like user input and other IO operations. By using monads, we can effectly handle IO actions within the game loop, ensuring that our functional approach remains intact while dealing with interactive elements. This will deepen our understanding of how Haskell handles state changes and IO in a purely functional context, helping us to develop clean, modular code that maintains Haskell's core principles.

## Proof of Concept (Matthew)

5: Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.
This need not go above-and-beyond, but we will have high expectations for this mark.
(I.e., we mean it when we say “fully functional”, easy-to-use/-review, and clear.)

Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)

The key elements the proof-of-concept focuse on are resource setup and window management for our game. In order to make a game,
it is imperative that we can create a simple and reliable GUI for our users to interact with. From the perspective of end users,
this is the most important part of a game. Due to Haskell's functional nature, and how cumbersome IO can be with functional languages, this is no trivial task. Developing a framework to tackle these problems is precisely what we aim to tackle with our proof-of-concept.

Our proof-of-concept accomplishes three vital components of our final game:

1. **SDL IO management**. As mentioned above, IO is generally confusing and difficult in functional languages. Consequently, documentation on the use of rendering libraries like SDL is generally limited for Haskell. In order to reconcile this issue, this proof-of-concept aims to abstract some of this functionality. Particularly, we have defined a [game loop](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/3b7869cbdb57d69e804bb8d062cafbe7fd7ca6c3/haskell/app/Main.hs#L27-L50) and [initialization functions](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/3b7869cbdb57d69e804bb8d062cafbe7fd7ca6c3/haskell/app/Main.hs#L17-L24) to setup window resources and our rendering pipeline. Abstracting some of the particularly difficult IO concepts to core helper functions will accelerate our development for future milestones. With the groundwork laid out in this proof-of-concept, we can focus on writing idiomatic Haskell going forward.

2. **Player state management**. Framing a game in an object-oriented mindset makes game development, and especially game design, much easier. This is more difficult in functional languages than some other object-oriented or procedural counterparts. In order to reconcile this, this proof-of-concept defines [data types](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/3b7869cbdb57d69e804bb8d062cafbe7fd7ca6c3/haskell/src/Move.hs#L6C1-L6C32) to help encapsulate the functionality of a player. In the future, this will include type classes as we begin to refactor and define what each type of object should be able to do in our game.

3. **Simple player movement framework**. Because we plan on making a game in which the player runs horizontally across the screen, we felt it was important to develop a simple proof-of-concept of this [functionality](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/3b7869cbdb57d69e804bb8d062cafbe7fd7ca6c3/haskell/src/Move.hs#L12C1-L22C19). This work on being able to move the player highlights the power of the abstractions outlined in 1 and 2, and how this makes writing game-logic easier for subsequent checkpoints.

Given the significant groundwork outlined above, we believe that subsequent work will come naturally from our framework, and the remainder of development will be simple.

## How to test and run the code: Haskell

In order to run our game, SDL2 must be installed. As such, the setup instructions have been separated by operating system.

### MacOS / Linux instructions

1. **[MacOS]** In order to install SDL2, we recommend using the popular "missing package manager" for MacOS: [homebrew](https://brew.sh/). If not already installed, please run the following command in your terminal (from their installation instructions on homebrew's website),
2. Ensure pkg-config is installed by running `pkg-config --version`. If not, install it using homebrew: `brew install pkg-config`.
3. Ensure haskell-stack is installed by running `stack --version`. If not, install it using homebrew: `brew install haskell-stack`.
4. For MacOS, install SDL2 with homebrew: `brew install sdl2`. You can check that this was installed correctly by running `brew info sdl2` (we're running version 2.30.8, as of writing). For Linux distributions, please install SDL2 using your favorite package manager of choice.
5. From the Haskell project root, run the game with `make rungame` (see the Makefile for more details on what this does). You can move the character (the red square) with the left and right arrow keys. To quit the window, press "q".
6. To run our unit tests for the movement logic, run `make test` from the Haskell project root.

**Note**: SDL2 does not [support opening windows](https://piazza.com/class/m0g7rf06fkhqb/post/147) from the interactive terminal (`ghci`), so please use `make rungame` in order to play the game.

### Windows instructions

Due to a known bug with stack install SDL2, it is quite difficult to get it working on Windows. Please use a Unix machine, emulator, or alternative with a robust package manager. The following crossed out instructions are here only for the sake of preserving this README's history; _they do not work_ properly after including the SDL2 extensions (image, mixer, ttf).

~~Install stack from the following link https://docs.haskellstack.org/en/stable/install_and_upgrade/#__tabbed_4_3~~

~~Run each line below one by one in terminal~~

~~``stack setup;`~~

~~`stack exec -- pacman -Syu`~~

~~`stack exec -- pacman -S mingw-w64-x86_64-pkg-config`~~

~~At this point normally one could run stack install SDL2, but due to a known bug described in the GitHub issue below it does not work. Instead, we have manually included the required SDL2 packages in our repo. Please complete the following instructions to complete the manual install. (Generally, we are following the workaround noted here: https://github.com/haskell-game/sdl2/issues/277#issuecomment-2283057736).~~

~~1. While in the root project directory, run the command: `stack exec -- bash` to open an interactive shell~~
~~2. Paste these 12 lines into the interactive shell, 3 for each package:~~

~~SDL2 package:~~

~~`cp SDL2-2.30.6/x86_64-w64-mingw32/lib/\* -r /mingw64/lib/`~~

~~`cp SDL2-2.30.6/x86_64-w64-mingw32/include/\* -r /mingw64/include/`~~

~~`cp SDL2-2.30.6/x86_64-w64-mingw32/bin/\* -r /mingw64/bin/`~~


~~SDL2_image package:~~

~~`cp SDL2_image-2.8.2/x86_64-w64-mingw32/lib/\* -r /mingw64/lib/`~~

~~`cp SDL2_image-2.8.2/x86_64-w64-mingw32/include/\* -r /mingw64/include/`~~

~~`cp SDL2_image-2.8.2/x86_64-w64-mingw32/bin/\* -r /mingw64/bin/`~~

~~SDL2_mixer package:~~

~~`cp SDL2_mixer-2.8.0/x86_64-w64-mingw32/lib/\* -r /mingw64/lib/`~~

~~`cp SDL2_mixer-2.8.0/x86_64-w64-mingw32/include/\* -r /mingw64/include/`~~

~~`cp SDL2_mixer-2.8.0/x86_64-w64-mingw32/bin/\* -r /mingw64/bin/`~~

~~SDL2_ttf package:~~

~~`cp SDL2_ttf-2.22.0/x86_64-w64-mingw32/lib/\* -r /mingw64/lib/`~~

~~`cp SDL2_ttf-2.22.0/x86_64-w64-mingw32/include/\* -r /mingw64/include/`~~

~~`cp SDL2_ttf-2.22.0/x86_64-w64-mingw32/bin/\* -r /mingw64/bin/`~~

~~3. You may now type `exit` to exit the interactive shell.~~
~~4. You should now be set up to play the game! From the `haskell` project root directory, run the game with command `make rungame` (see the Makefile for more details on what this does). You can move the character with the left and right arrow keys. To quit the window, press `q`.~~
~~5. Optionally, you can run our unit tests for the movement logic by typing command `make test` from the Haskell project root directory~~
