# TwitchyDemo

Background:
This demo is my interpretation of what 'Gaming with Twitch' is implemented, there is a server which takes in user commands and sends the most popular command to control some character. However the 'character' for this project is a robot. Communication with the robot is done through serial/UART. The project acts like a chatroom for the most part. What functionallity that has been added is the server will use STMs to keep a queue which is filled with client requests. The server will the interpret the queue and send the most popular command to the robot. The project is done in Haskell in full, and shows a practical application of both concepts in functional languages and functional languages themselves.


Note:
This demo is meant to run on a Raspberry Pi (due to the dependency on /dev/sttyAMA0).
You can change this for whatever serial port you desire.

This project was built and compiled in a raspberry pi. This means that it's GHC and cabal-install are not updated to the latest verions. So I did not have the luxury of setting up a cabal managed project. I would not be surprised that there is a work around as of yesterday. 

--------------------------------------------
Setup:

PREREQS: ghc (version >= 7.4), cabal-install (version >= 1.14)

clone the repo into where ever.

ghc Main

./Main


To Use: 
nc localhost 4000 

--------------------------------------------

What needs to be improved for this project:

Graceful exit of the program.

More effective commenting. 

cabal....

