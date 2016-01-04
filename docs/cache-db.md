# Cache db

## Rationale

With the normalized db (see [git-db](git-db.md)) stored in git, all
the data is correctly stored. However, it's definitely not performant
for e.g. sorting or filtering. For example, if you want to list the 10
last issues by date, you have to load all the issues, sort them, and
keep the 10 first. This is extremely inefficient.

A database is something that is *made* to handle this properly.

## Proposal

An sqlite database could be stored in
`.git/refs/zed/issues/cache.db`. This sqlite database would be updated
when necessary, and any query should go to the cache db instead of
querying git files.

This solves the sorting/filtering issue, and the general performance
issue fetching in git db could introduce. It does introduce, however,
the following question: when should the db be updated?

### Updates

There are 2 kinds of updates:

- Minor update: the user just created an issue or replied to an
  existing one, so we just need to insert the new entry in the db.
- Full update: the user just updated from a remote. In this case, we
  need to update the whole database, since we don't know what
  changed.[^1]

The issue is not knowing *when* a user is updating from a remote. If
the user just runs `git fetch`, zed isn't aware of it.

There are several solutions:

1. Add a git post-fetch hook. An extension shouldn't mess with hooks
   though.
2. Manually run the update. Not very user-friendly.
3. Every time `git zed` is run, check the mtime of the db and the mtime
   of the head. If the db's is older than head's, rebuild
   everything. It originally looks like the best solution, but the user
   isn't always running `git zed`. For example, if he opened `instaweb`
   (or whatever the equivalent will be), then refreshing his browser
   won't get the latest issues.
4. Providing a `git zed` command to update the issues. This command
   will basically run `git fetch <remote>
   refs/zed/issues/head:refs/zed/issues/head` and then run the
   rebuild. The drawback is that users running `git fetch` manually
   will need to update the db manually. A separate command to only run
   the rebuild would solve this though. If a user is savvy enough to
   know which fetch command to run, assuming it should know about the
   cache db is not a far fetch.


[^1]: This could be updated to use the diff algorithms of git to fix
    this. For now though, the simplest is to just rebuild everything.
