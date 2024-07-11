#!/usr/bin/env bash
set -e
echo "$PGP_SECRET" | base64 -d -i - > /tmp/signing-key.gpg
echo "$PGP_PASSPHRASE" | gpg --pinentry-mode loopback --passphrase-fd 0 --import /tmp/signing-key.gpg
(echo "$PGP_PASSPHRASE"; echo; echo) | gpg --command-fd 0 --pinentry-mode loopback --change-passphrase $(gpg --list-secret-keys --with-colons 2> /dev/null | grep '^sec:' | cut --delimiter ':' --fields 5 | tail -n 1)
