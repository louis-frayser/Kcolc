# Kcolc

**Kcolc is "Clock" (in reverse)**


## OVERVIEW

Until I can find a good generic timer application for Linux, I'm rolling my own. The plan is to prototype a countdown timer in various ways, beginning with an application in Racket Scheme. Ultimately there will be an app that has big numbers and is usable on any Linux X desktop--think of X Dali Clock, but as a timer. The functionality is what you see in those web-based timers when Googling *countdown timer*.


## STATUS

The prototype written in Racket scheme already functions as a countdown timer.  The next step is to add an action to run an arbitrary user command upon completion of the countdown.  Currently there is a beep sound upon timeout, and that requires the *rsound* package, installable with **rako pkg install -i rsound**. The **-i** is necessary for system-wide use, ie global install.

![Screenshot](docs/Screenshot%20at%202018-10-04%2005-06-29.png)
