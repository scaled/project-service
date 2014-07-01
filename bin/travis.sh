#!/bin/sh
#
# Builds and tests (for travis-ci.org)

# create and clean our temp build directory
mkdir -p target/spam
SPAM=`cd target/spam ; pwd`
rm -rf $SPAM/*
cd $SPAM

# download the spam script
rm -f spam
wget https://raw.githubusercontent.com/scaled/pacman/master/bin/spam
chmod a+rx spam

# install/build the packages
./spam -d -Dscaled.meta=$SPAM install project-service
./spam -d -Dscaled.meta=$SPAM install java-project
./spam -d -Dscaled.meta=$SPAM install scala-project
./spam -d -Dscaled.meta=$SPAM install maven-project
./spam -d -Dscaled.meta=$SPAM install sbt-project
