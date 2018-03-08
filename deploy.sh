#!/bin/bash

# Quit on error
set -e

# Stash any uncommitted changes
git stash

# Clean out the cache and rebuild the site from the current branch
stack exec site rebuild

# Switch to master branch and make sure it's ready to be pushed to
git checkout master
git pull

# Update the site files
rsync -a --filter='P _site/'      \
         --filter='P _cache/'     \
         --filter='P .git/'       \
         --filter='P .gitignore'  \
         --filter='P .stack-work' \
         --delete-excluded        \
         _site/ .

# Deploy the site
git add -A
git commit -m "Publish $(date -Is)."
git push

# Return to the previous branch and restore any stashed changes
git checkout -
git stash pop
