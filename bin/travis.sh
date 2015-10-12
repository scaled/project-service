#!/bin/sh
#
# Builds and tests (for travis-ci.org)

wget https://raw.githubusercontent.com/scaled/pacman/master/bin/build-test.sh

# build and test a bunch of project-related packages
GITROOT=git:https://github.com/scaled/
sh build-test.sh $GITROOT/project-service.git
sh build-test.sh $GITROOT/java-project.git
sh build-test.sh $GITROOT/scala-project.git
sh build-test.sh $GITROOT/maven-project.git
sh build-test.sh $GITROOT/sbt-project.git

rm build-test.sh
