# TwitchyDemo

Background:
This demo is my interpretation of what 'Gaming with Twitch' is implemented. The project is done in Haskell in full.
It also shows a practical applications of both concepts in functional languages and functional languages themselves.


Note:
This demo is meant to run on a Raspberry Pi (due to the dependency on /dev/sttyAMA0).
You can change this for whatever serial port you desire.

This project was built and compiled in a raspberry pi. This means that it's GHC and cabal-install are not updated to the latest verions. So I did not have the luxury of setting up a cabal managed project. I would not be surprised that there is a work around as of yesterday. 

Setup:
<REREQS: ghc (version >= 7.4), cabal-install (version >= 1.14)>

clone the repo into where ever.
ghc Main
./Main

What needs to be improved for this project:
Graceful exit of the program. (I'm lazy)
More effective commenting. 
cabal....

