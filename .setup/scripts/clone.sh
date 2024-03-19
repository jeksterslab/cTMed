#!/bin/bash

git clone git@github.com:jeksterslab/cTMed.git
rm -rf "$PWD.git"
mv cTMed/.git "$PWD"
rm -rf cTMed
