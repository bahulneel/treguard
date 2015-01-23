#!/bin/sh

cd docs
git pull origin gh-pages
cd ..
lein trampoline marg -f index.html
cd docs
git commit -am "Updated documentation"
git push origin gh-pages
cd ..

