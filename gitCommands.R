# go to the repo directory (if not already there)
cd sportsScheduler

# confirm remote is set correctly
git remote -v
# should show:
# origin https://github.com/illusionsorders/sportsScheduler.git (fetch)
# origin https://github.com/illusionsorders/sportsScheduler.git (push)

# check what changed
git status

# stage all changes
git add .

# commit changes
git commit -m "Started interfaceWithoutAPI"

# push to GitHub
git push origin main
