# /etc/zsh/zshenv: system-wide .zshenv file for zsh(1).
#
# This file is sourced on all invocations of the shell.
# If the -f flag is present or if the NO_RCS option is
# set within this file, all other initialization files
# are skipped.
#
# This file should contain commands to set the command
# search path, plus other important environment variables.
# This file should not contain commands that produce
# output or assume the shell is attached to a tty.
#
# Global Order: zshenv, zprofile, zshrc, zlogin

source /etc/profile

export PATH=/home/avru/bin:/home/avru/scripts:$PATH
export PATH=/opt/MATLAB/R2013a/bin:$PATH

export BROWSER="firefox-nightly"
export EDITOR="emacsclient_launcher.sh"
export VISUAL="emacsclient_launcher.sh"
export ALTERNATE_EDITOR="vim"

export LESS=" -R "
export LESSOPEN="|lesspipe.sh %s"
export PAGER="less"

export WINEDEBUG=-all
export pacman_program="yaourt"
export VDPAU_DRIVER=va_gl

# Kill flow control
if tty -s ; then
    stty -ixon
    stty -ixoff
fi
