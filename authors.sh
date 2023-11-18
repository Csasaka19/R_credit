#!/bin/bash

# Get list of authors ordered by number of commits
git log --format='%aN <%aE>' | sort | uniq -c > AUTHORS
