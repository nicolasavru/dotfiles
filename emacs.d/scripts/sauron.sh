#!/bin/bash

CHECKDIR="/home/$LOGNAME/Maildir/INBOX"
sauron-msg () {
DBUS_COOKIE="/home/$LOGNAME/.sauron-dbus"
if test "x$DBUS_SESSION_BUS_ADDRESS" = "x"; then
    if test -e $DBUS_COOKIE; then
        export DBUS_SESSION_BUS_ADDRESS="`cat $DBUS_COOKIE`"
    fi
fi
if test -n "x$DBUS_SESSION_BUS_ADDRESS"; then
    dbus-send --session           \
        --dest="org.gnu.Emacs"    \
        --type=method_call        \
        "/org/gnu/Emacs/Sauron"   \
        "org.gnu.Emacs.Sauron.AddMsgEvent"    \
        string:shell uint32:3 string:"$1"
fi
}
#
# -mmin -5: consider only messages that were created / changed in the
# the last 5 minutes
for f in `find $CHECKDIR -mmin -5 -a -type f`; do
    sender=`mu view $f | grep -i '^From:' | sed 's/^From://'`
    subject=`mu view $f | grep -i '^Subject:' | sed 's/^Subject://'`
    sauron-msg "mail: $sender - $subject"
done
