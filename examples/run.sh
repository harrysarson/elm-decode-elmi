#!/bin/bash

set -e

elm make src/Main.elm --output elm.js

node main
