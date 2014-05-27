colourlogcat
============

Colour adb logcat to make it more readable.

*Haskell version*

cabal build

adb logcat | ./dist/build/colourlogcat/colourlogcat

Or put the executable somewhere better.

*Scala version*

adb logcat | ./colourlogcat.scala

You need Scala.


*Ruby version*

adb logcat | ./colourlogcat.rb

You need the colorize gem, and an OS where 'tput cols' works.
