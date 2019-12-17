# bytemunch

This is an example application that demonstrates how `sockets` can be
used with systemd activation sockets. There is a `service` file and
a `socket` file in this directory for use with systemd. To try this
out, copy both files to `~/.config/systemd/system/`. From the top-level
directory of the sockets library run:

    cabal build --write-ghc-environment-files=always
    ghc -threaded examples/bytemunch/bytemunch.hs
    cp examples/bytemunch/bytemunch ~/.local/bin/

Activate the service and then start feeding in bytes:

    systemctl --user start bytemunch
    echo "Hello World" > /dev/tcp/localhost/9999
