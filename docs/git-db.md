# How to abuse git as a database

## Some basics

We're going to use 3 of the git object types available: blobs, trees
and commits.

Blobs are objects where you can put in some data, and trees are
objects where you can have a list of blobs or trees. A commit points
to a tree.

Here's how to create a blob:

```
$ echo 'foo bar baz' | git hash-object -w --stdin
```

This will print to stdout the hash of the new blob.

Here is how to create a tree:

```
$ printf '100644 blob <hash>\t<filename>' | git mktree
```

This will print to stdout the hash of the new tree.

Here is how to create a commit:

```
$ git commit-tree [-p <parent>] -m 'message' <tree hash>
```

This will print to stdout the hash of the new commit.

## Where?

The issues are going to be stored as objects in git's database. The
question is: how do we get them? Where are they?

The list of issues will be stored in a tree. This tree will look like
this:

```
100644 tree <issue hash>\t<issue hash>
100644 tree <issue hash>\t<issue hash>
```

The hash of the latest commit pointing to this tree of trees will be
stored in `.git/refs/zed/issues/head`.

So a simple command to get the list of issues will be this:

```
$ git ls-tree $(cat .git/refs/issues)
100644 tree <issue hash>\t<issue hash>
100644 tree <issue hash>\t<issue hash>
$
```

## The Types

There are 3 types in this project: list of issues (as seen above), an
issue, and an issue message.

### Issue

An issue is what's linked by the list of issues. It is a git tree that
will always look like this:

```
100644 blob <hash>\ttitle
100644 blob <hash>\tstatus
100644 blob <hash>\ttags
100644 tree <hash>\thead
```

Or, in a simpler way:

```
blob title
blob status
blob tags
tree head
```

The blobs are fairly straightforward in what they will contain. The
`tags` will just be a list separated by spaces.

Generally speaking, this way lets us be pretty flexible in what we
want to store in an issue.

The `head` tree is a link to the first message of the issue.

### Message

The message is another tree of this kind (in the simple form):

```
blob author
blob date
blob content
tree children
```

Most of it is pretty straightforward too.

The `children` tree is a list of trees pointing to children
messages. The filenames will be the hashes of the trees.

This means there will always be one root message for an issue (the
`head`). Then this message will have children, and each child can have
other children too. This is essentially like reddit's messages. (It's
the concept of "threaded messages".)

## Commits

I said in the first sentence that commits would be used.

Commits give several advantages and are actually mandatory:

- `git gc` would throw away our trees and blobs because they're
  unreachable without a commit pointing to them.
- They give us history over every change.

It's possible to automate the message to say something meaningful such
as "created issue foo" or "replied to issue bar".

If the history grows too much, a new command to shrink it down can be added.

## Tips & Tricks

Q: Why are we not using list of hashes in blobs instead of trees? We'd
gain so much space!

A: Indeed, but the main advantage of using trees is that git will handle
merges on its own. If 2 issues are created on 2 different clones, when
they'll merge, there won't be any conflict if it's 2 new files. There
will be conflicts if it's a newline in the same blob on both sides.

Q: How do I push that? Github supports it?

A: Just run git push origin refs/issues:refs/issues and git fetch
origin refs/issues:refs/issues. Github absolutely supports it.
