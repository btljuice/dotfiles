#!/bin/sh

# See http://pages.github.lab.mtl/Hopper/hopper-docs/onboarding/system-setup.html for the complete system setup
# Some stuff remains to be done manually
# - Install XCode
# - Add SSH key to github
# - Create and Artifactory key

echo "Setting up your Mac..."

# Check for Homebrew and install if we don't have it
if test ! $(which brew); then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update

# My own editors
brew cask install emacs
brew cask install macvim
brew cask install slack

# Software requirements for hopper development
brew install bash
brew install bash-completion
brew install coreutils
brew install git
brew cask install miniconda
brew cask install homebrew/cask-versions/adoptopenjdk8

# NOTE: Following scala installation:
#    ==> Caveats
#    To use with IntelliJ, set the Scala home to:
#    /usr/local/opt/scala/idea
brew install scala

# NOTE: Following sbt installation:
#    ==> Caveats
#    You can use $SBT_OPTS to pass additional JVM options to sbt.
#    Project specific options should be placed in .sbtopts in the root of your project.
#    Global settings should be placed in /usr/local/etc/sbtopts
brew install sbt@1

# MANUAL - setup the .gitconfig files.
# git config --global user.name "Alexis Trudeau"
# git config --global user.email "atrudeau@hopper.com"
# git config --global core.editor "vim"


