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

#### My own editors/ide tools
brew cask install slack
brew cask install emacs
brew cask install macvim
brew cask install intellij-idea-ce
brew install the_silver_searcher
brew cask install iterm2


#### Debugging tools
brew cask install charles
brew cask install postman

#### Software requirements for hopper development
brew install bash
brew install bash-completion
brew install coreutils
brew install git
brew cask install miniconda
brew cask install homebrew/cask-versions/adoptopenjdk8

brew install scala
# NOTE: Following scala installation:
#    ==> Caveats
#    To use with IntelliJ, set the Scala home to:
#    /usr/local/opt/scala/idea

brew install sbt@1
# NOTE: Following sbt installation:
#    ==> Caveats
#    You can use $SBT_OPTS to pass additional JVM options to sbt.
#    Project specific options should be placed in .sbtopts in the root of your project.
#    Global settings should be placed in /usr/local/etc/sbtopts

brew install yarn
# NOTE: Following yarn installation, about icu4c dependency
#    icu4c is keg-only, which means it was not symlinked into /usr/local,
#    because macOS provides libicucore.dylib (but nothing else).
#
#    If you need to have icu4c first in your PATH run:
#    echo 'export PATH="/usr/local/opt/icu4c/bin:$PATH"' >> ~/.bash_profile
#    echo 'export PATH="/usr/local/opt/icu4c/sbin:$PATH"' >> ~/.bash_profile
#
#    For compilers to find icu4c you may need to set:
#    export LDFLAGS="-L/usr/local/opt/icu4c/lib"
#    export CPPFLAGS="-I/usr/local/opt/icu4c/include"
yarn global add bower gulp-cli

brew install nvm


# MANUAL - setup the .gitconfig files.
# git config --global user.name "Alexis Trudeau"
# git config --global user.email "atrudeau@hopper.com"
# git config --global core.editor "vim"


