## TerminalTODO

A simple TODO application that prints your TODO items to the terminal whenever you login (See below).

# Building and Installing

Configuring and building is simple with cabal:

`
# Step by step
cabal configure
cabal build
cabal install

# Or
cabal install
`

After the above commands, you can view your TODO items and other info by running `todo`.

# Setting up automatic printing

In order to get the todo information to your terminal you need to add the following command to
your `~/.bash_profile` file (running the install.sh script will accomplish this if you're lazy):

`todo -v`

This will be executed every time a login shell is created. If you are using Mac OS X's Terminal.app
or are logging in via SSH, `~/.bash_profile` is run each time you open a new window or each time you
remotely login.

For other terminal emulators, this may still work or, in the case that they use non-login shells,
you may have to add the command to your `~/.bashrc` file.

# Editing TODO items

Running todo with the `-n` option will fork and exec the editor specified by the $EDITOR environment
variable. It will fail if $EDITOR is not set.

Within the editor you can add TODO items in the following format:

`
# Comments start with hash marks
Item Title|01/01/2014|This is a description! Finish descripti
`
