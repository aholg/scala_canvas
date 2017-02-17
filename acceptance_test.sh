#!/usr/bin/env bash

echo "Starting acceptance test"

diff  $(bash acceptance_commands.sh) "acceptance_output.txt"
