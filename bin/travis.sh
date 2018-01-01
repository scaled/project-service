#!/bin/sh
#
# Builds and tests (for travis-ci.org)

set -e
wget https://raw.githubusercontent.com/scaled/pacman/master/bin/build-test.sh

# build and test a bunch of project-related packages
GITROOT=git:https://github.com/scaled
sh build-test.sh \
  $GITROOT/project-service.git \
  $GITROOT/java-mode.git \
  $GITROOT/scala-mode.git \
  $GITROOT/maven-project.git \
  $GITROOT/sbt-project.git

rm build-test.sh
