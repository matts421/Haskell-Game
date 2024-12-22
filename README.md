# A Snail's Adventure!

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

## Team Haskell

Our team (named Haskell) is:

- Matthew Smith (51209682)
- Mo Fardinzaman (57779167)
- Nicolas Mercuri (18981167)
- Danielle Lim (33485137)
- Devin Chikhlia (80666621)

## Acknowledgments

We surely built on the work of others! Here are resources and people we got support from:

- The quickstart guide in Haskell's [game-dev page](https://haskell-game.dev/packages/sdl2.html)
- This [game](https://github.com/JeremiahCheatham/Yellow-Snow/blob/main/Haskell-SDL2/app/Main.hs) was used as inspiration from this, and as a reference for Haskell SDL2 examples
- Haskell Wiki's instructions on installing SDL2 on Windows https://wiki.haskell.org/SDL/Windows
- Windows SDL2 installation via stack https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/
- GitHub issue for windows sdl2 installation via stack https://github.com/haskell-game/sdl2/issues/277

### Assets

All game assets used are available for use on this project under their respective licenses.

- Background music: https://madmakingmistery.itch.io/jungle-confusion
- SFX: https://spikyandrew.itch.io/playful-8-bit-sounds-and-music
- Snail sprite: https://dustdfg.itch.io/pixel-art-snail
- Stickbug sprite: https://gamebetweenthelines.itch.io/animated-side-scroller-stick-bug-enemy
- Bird sprite: https://ankousse26.itch.io/bird-flying-pixel-art-animation-free
- Background and tileset: https://jesse-m.itch.io/jungle-pack?download

## Proposal

Take a look at our original project proposal [here](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/tree/main/haskell#:~:text=2%20months%20ago-,README.md,-Infinite%20Runner%20Game).

## Video Demo

Take a look at a video demo on youtube [here](https://www.youtube.com/watch?v=GmSql6xqbA4).

## Guide to the MVP

### How the MVP fulfills the proposal

The MVP set out to complete these tasks:

- A single type of obstacle moving at a static pace
- A basic scoring system that increments over time
- Simple collision detection
- Game over detection when the player collides with an obstacle
- The ability to restart the game after losing

Here is how we have accomplished these goals:

1. The game spawns an enemy (the stickbug), and assigns it a random x-location on the right of the screen, and a random speed. This pace does not change after it is spawned.
2. The game implements a basic scoring system, which is related to the number of seconds the player has currently survived. This is displayed on screen, and counts up every second.
3. Every frame, the game update queries every enemy to check whether or not they are currently colliding with the player. This is done through implicit equations determining if the player rectangle and enemy rectangle intersect. This is how we define collisions in our game.
4. When a collision occurs, we end the game loop, and send the player to the title screen. This is our "game-over detection".
5. On the title screen and game-over screen, the player has the choice between quitting the application, or trying again to beat their previous score.

We took several significant steps beyond our (core) proposed MVP. Notably,

6. Added an additional enemy (the bird). This adds an additional gameplay element to our game, and makes it more exciting to play
7. Added high-score functionality, allowing users to persist their best score to disk, and try their best at beating their old records.
8. Scaling difficulty levels that make enemy spawns more common, and vary enemy type as the game progresses. This rewards players with a more exciting gameplay experience as their run progresses.
9. Added a wide array of quality of life features to make the game very fun and visually appealing. These include:
   - A multi-layer parallax side-scrolling background to give a pseudo-3D effect to the game
   - Player, background, stickbug, and bird animations to give the game more life
   - Sound effects for confirming starting the game, quitting the game, jumping, leveling-up (reaching a new difficulty threshold), and dying
   - A jungle-themed looping background track that immerses the player in the environment

All of these features take our MVP above and beyond our core proposed MVP, making our game robust, visually appealing, and tons of fun.

### Code links

The code links for the numbered sections above are as follows:

1. [First enemy](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Enemy.hs#L18C1-L56C6)
2. [Scoring system](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/UI.hs#L1-L35)
3. [Collision system](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Game.hs#L78-L91)
4. [Game-over](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/app/Main.hs#L138-L141)
5. [Retry screen (code is shared with title-screen)](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Title.hs#L17-L76)
6. [Additional enemy](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Enemy.hs#L58-L97)
7. [High-score](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Game.hs#L66-L75)
8. [Difficulty](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Game.hs#L54-L64)
9. Sounds and Visuals
   - [Background implementation](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Background.hs#L12-L60)
   - [Textures](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Texture.hs#L1-L76) and [animations](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Entity.hs#L19-L31)
   - [Sound effects](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/src/Music.hs#L12-L24)
   - [Background music](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/99bdfcb71bb79d5ef9d5ce75a7178aa8987f0d30/haskell/app/Main.hs#L79-L80)

### How to run the MVP

#### Installation

1. We recommend using the popular "missing package manager" for MacOS: homebrew. If not already installed, please follow the instructions listed on their [website](https://brew.sh/).
2. Ensure pkg-config is installed by running `pkg-config --version`. If not, install it using homebrew: `brew install pkg-config`.
3. Ensure haskell-stack is installed by running `stack --version`. If not, install it using homebrew: `brew install haskell-stack`.
4. Install SDL2: `brew install sdl2`.
5. Install SDL2_mixer: `brew install sdl2_mixer`
6. Install SDL2_image: `brew install sdl2_image`
7. Install SDL2_ttf: `brew install sdl2_ttf`

_Note_: you can follow the steps above using a different package manager of choice, if desired

### Playing the game

To start the game, run `make rungame` from the haskell project root. Press spacebar or up-arrow to jump in-game!

## Guide to new learning

### What we learned

We dove deeper on the following concepts in Haskell:

1. Numbers
   - In our project, we explored and learned lots about differnt Numeral classes in Haskell. This required us to dive deep into the world of `Integer`, `CInt`, `Float`, `Double`, and `Word32`. While seemingly innocuous from a first glance, simple conversions between these sorts of types is taken for granted when writing code in other languages like Python. Serious thought went into the choice of the different number class for the fields in all of our data structures in this project so as to conform to existing APIs in SDL, and also to support the necessary amount of precision for seamless movement, animation, and score counting.
2. Random number generation
   - Random number generation is another concept that is often overlooked as trivial in many procedural languages. Generally speaking, these languages store some sort of global RNG seed state so as to ensure the pseudo-random number generator produces (basically) unique sequences every time a random number is requested. In a functional language however, this is a _huge_ red flag. How can a _function_ with the same output return a different (random number) upon subsequent calls? Of course, this is the desired effect, so Haskell solves this (like many other "weird" programming things in functional languages) with monads and IO. We encourage the reader to think critically about this: isn't it weird that random numbers are related to _IO_ (minds blown weekly ™️)?
3. Abstraction and data structures
   - When undergoing a software project, our group members longed for some of the familiar and handy language features we know and love. The most notable example are classes, and to a similar extent, structs. We dove deeper on this topic in Haskell, and discovered the handy "record" syntax. This syntax allowed us to easily create data-classes with _named_ built-in getters. Moreover, we combined these record data-classes with type-classes to provide interfaces and abstract method implementations. This made development a lot smoother, more robust, and familiar.
   - In order to make performance optimizations, we pre-rendered many textures in a `TextureMap` data structure. This allowed us to reap the rewards of not having to recreate these objects through slow IO operations every single frame.
4. Lots and lots and lots of IO (yay Monads!)
   - By design, a game requires a lot of IO. Naturally, this required us to learn not only a lot about IO in Haskell, but also the SDL library in general. In doing this, we read lots of SDL documentation in order to manage the external library. We also learned a lot about various syntax features and quirks in Haskell (such as `do` and other features from `Control Mondads`).

### Code links

The code links for the numbered sections above are as follows:

1. Numbers
   - [Conversion between Word32 and CInt](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Settings.hs#L19-L20)
   - [Conversion between CInts and Doubles](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Settings.hs#L22-L23)
   - [Example of number diversity in code](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Entity.hs#L33-L38)
   - [Double-point precision for physics and gravity in player movement](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Player.hs#L42-L100)
2. Random number generation
   - [Random speed of enemies](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Enemy.hs#L93)
   - [Random position of enemies](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Enemy.hs#L89)
   - [Random enemy type generation](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Enemy.hs#L85-L87)
3. Data structures
   - [Record syntax use for textures](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Texture.hs#L28-L76)
   - [Entity type class](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Entity.hs#L9-L31)
   - [Extension of entity by player](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Player.hs#L38-L100)
   - [Extension of entity by enemy](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Enemy.hs#L23-L42)
4. IO
   - [SDL resource creation](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/app/Main.hs#L27-L43)
   - [SDL resource clean-up](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/app/Main.hs#L45-L48)
   - [Playing music](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Music.hs#L17-L24)
   - [Rendering to screen](https://github.students.cs.ubc.ca/magsubc/cpsc-312-project/blob/f67c6a502d1aa4b09aaae0be26703dca7f5fc7ce/haskell/src/Background.hs#L17-L60)

### How the project benefits from our new learning

The project benefits follow,

1. Numbers
   - The careful choice of numbers and their conversion is vital for our project. All of our game updates operate with a `deltaTime` parameter. This allows the game to run identically, independent of frame-rate. This allows users of our game to have a similar experience, regardless of how fast their computers are. Additionally, these decisions are vital for the physics performance of the player and enemies, as well as pixel-perfect rendering of the textures
2. Random number generation
   - While relatively small in terms of line-number, random number generation is the true keystone of our project. This is one of the main gameplay elements, making the gameplay experience fun with a high amount of replayability. It also makes the game more visually appealing and less robotic; hence better emulating the jungle scene on the screen.
3. Data structures
   - The data structures and code abstraction used above was vital for our project completion and team collaboration. By making easy-to-use and robust APIs we have made our code easily extensible for future features, and also easy to test, debug, and work on asynchonrously in a team-environment.
4. IO
   - Without deliberate IO management and use within our project, we would not have a project at all. SDL is the core of our project, and is directly required for audio management, rendering to the screen, and taking keyboard input to control the player.
