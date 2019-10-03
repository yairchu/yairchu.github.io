git checkout master
git reset --hard HEAD@{1}
git rm -r .
git checkout HEAD -- docs
git mv docs/* .
git commit -m"Publish"
git push --force
git checkout -
