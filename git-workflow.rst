Git Overview
------------

git is a version management tool. A repository is a directory tree
with a `.git` directory at the root. Git stores all its data in
`.git` directory.  Files within that directory tree can be added to
git. Contents of the files added to git are stored by git in `.git`
directory. When one or more files in the tree are modified, git keeps
the old versions of the files intact. We can view the new changes
as a diff wrt to the old versions. New changes to the files can be
`committed` to the git repository as a single changeset which creates
new versions of all the modified files keeping the old versions
intact. Each such changeset or commit is given a commit-id and it can
be viewed later as a diff from the old versions. All changes to the
repository from its beginning to present time can be viewed as a log.

Creating a new repository
-------------------------

::

    $ cd repo    # go to the dir to be made into a repo
    $ git init   # create a new repo
    $ git add .  # add the current dir tree for committing to the repo
    $ git commit # commit the added changes to the repo

The directory ``repo`` is also called a workspace. ``git`` stores all
its data in the `.git` directory at the root of the workspace. If you
remove the `.git` directory the workspace just becomes a plain regulary
directory tree with no git awareness.

Cloning an existing repository
------------------------------

::

    # clone streamly repo on github to "streamly" dir in current dir
    $ git clone https://github.com/composewell/streamly

    # clone a local repo
    $ git clone repo repo1

The newly cloned repo is known as the "local" repository and the one
you cloned from is the "remote" repository. The remote repository is
associated to the local repo by the name `origin` by default::

    $ git remote -v

After cloning both the repos have exactly the same content. Later, both the
repos can change their contents independently. Someone can make new changes to
the remote repo, and we can make some new changes to our local repo. We can
push our local changes to the remote repo by using `git push` and we can pull
new changes from the remote repo using `git pull`::

    $ git push origin master    # push changes on branch master to "origin"
    $ git push origin my-branch # push the changes on  branch my-branch to "origin"
    $ git pull                  # pulls from origin by default

Set ``git config --global pull.ff only`` so that we do not accidentally merge.

Modifying files
---------------

Change the files in a repo just as usual using your editor. When you modify a
file only the "checked out" copy in your dir tree is modified. The "checked in"
or "committed" copy that is stored in the ".git" directory remains intact. To
see what all is modified::

    $ git status

You can see the difference between the committed copy and the checked
out copy using::

    $ git diff          # textual diff of all modified files
    $ git diff file     # textual diff of the specified file
    $ git difftool      # diff all modified files in your workspace
    $ git difftool file # diff just the specified file

Committing changes
------------------

The modified files can be committed to the repository to make the changes
permanent and recorded as a revision in the repository::

    $ git add file1  # Add the file to the set of files to be committed
    $ git add file2  # Add the file to the set of files to be committed
    $ git status     # See which files are to be committed
    $ git commit     # commit the files

If you want to discard the changes made to a file::

    $ git checkout file

It will discard any changes made and restore the file to the previously
committed version.

To list all files added to the repository::

    $ git ls-files

To see the commit history of a file::

    $ git log file

The log shows the commit-ids of the commits to the file till now.  To
check the diff from a particular commit till now::

    $ git difftool commit-id

To check the diff from the commit previous to a give commit and that commit::

    $ git difftool commit-id^ commit-id

To check the diff between two commits::

    $ git commit-id1 commit-id2

Commit Messages
---------------

Read these:
* https://commit.style/
* https://chris.beams.io/posts/git-commit/#seven-rules .

Creating branches
-----------------

By default there is only one branch in the repository called "master". By
default you are working on the master branch. To see which branch you are on::

    $ git status

When new changes are made to files in the repository they are recorded
as changes on the master branch. The master branch moves forward as new
changes are committed. The latest commit on the branch is called the HEAD
commit.

You can see all the commits to the repository on the current branch using::

    $ git log

All the commits to a specific file using::

    $ git log file

You can create a new branch off some commit on the master branch. This means
all the files/commits up to that point will also be available on the new
branch. When we make any changes to the files on a branch no other branches are
affected, those changes are visible only on that branch. In other words we have
cloned a branch from the beginning to the given point including all the commit
history of that branch and now we can make changes to it independently. To
create a branch::

    $ git branch test  # creates a branch from the current branch HEAD commit

By default we are on the "master" branch. To switch to a branch::

    $ git checkout test -- switch to test branch

To know which branch we are on::

    $ git status

When you switch to a new branch all the files in the workspace get replaced by
the latest versions on that branch. To switch back to master branch::

    $ git checkout master -- switch to master branch

To see the commits different between master and your branch::

    $ git log master..test

To see the history and relationships of all branches and which commit
the branch is forked from etc. use this command::

    $ git log --graph --decorate --pretty=format:'%C(auto)%h %cd %d %s' --date=short --all 

If you  want to see the difference of the current branch from some other
branch e.g. master::

    $ git difftool master # show the diff of current with master

Forking on github
-----------------

On github when you fork a repository, your fork would have the original
repository set as a remote named `upstream`. When you clone your fork
then you have two remotes in the cloned repo, the original repository is
named `upstream` and your forked repository is named `origin`.

Making new changes
------------------

After forking/cloning a repo you need to make changes to the repo,
then push your changes to your fork and create a pull request or merge
request to the original repository.

Using the fork model, original repo branch model.

Do not work on the master branch.
create a new branch usually from master.

refreshing your local repo with changes on upstream repo.
rebasing your changes on top of newly pulled changes.

Merging
-------

The most common case of non-maintainer merge is during rebase.

Rebasing
--------

rebasing on top of new changes to master/some other branch.
rebasing to squash/drop/reorder commits.

Stashing
--------

Others
------

git rm/mv/reset/show/cherry-pick

References
----------

* https://git-scm.com/docs/gittutorial
* https://git-scm.com/docs/giteveryday
* https://git-scm.com/docs/gitworkflows
* https://git-scm.com/docs reference
* https://git-scm.com/doc all docs
